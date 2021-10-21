load("@bazel_skylib//lib:paths.bzl", "paths")
load(
    "//haskell:cc.bzl",
    "cc_interop_info",
)
load(
    "//haskell:private/context.bzl",
    "haskell_context",
)
load(
    "//haskell:private/cc_libraries.bzl",
    "get_ghci_library_files",
    "link_libraries",
)
load(
    "//haskell:private/dependencies.bzl",
    "gather_dep_info",
    "module_files_for_build",
)
load(
    "//haskell:private/expansions.bzl",
    "expand_make_variables",
)
load(
    "//haskell:private/mode.bzl",
    "is_profiling_enabled",
)
load(
    "//haskell:private/packages.bzl",
    "expose_packages",
    "pkg_info_to_compile_flags",
)
load(
    "//haskell:private/plugins.bzl",
    "resolve_plugin_tools",
)
load(
    "//haskell:providers.bzl",
    "GhcPluginInfo",
    "all_dependencies_package_ids",
)
load(
    "//haskell/experimental:providers.bzl",
    "HaskellModuleInfo",
)
load(
    "@bazel_skylib//rules:common_settings.bzl",
    "BuildSettingInfo",
)

def _expand_make_variables(name, ctx, strings):
    # All labels in all attributes should be location-expandable.
    extra_label_attrs = [
        [ctx.attr.src],
        ctx.attr.extra_srcs,
        ctx.attr.plugins,
        ctx.attr.tools,
    ]
    return expand_make_variables(name, ctx, strings, extra_label_attrs)

def haskell_module_impl(ctx):
    # Obtain toolchains
    hs = haskell_context(ctx)
    cc = cc_interop_info(ctx)
    posix = ctx.toolchains["@rules_sh//sh/posix:toolchain_type"]

    # Collect dependencies
    src = ctx.file.src
    extra_srcs = ctx.files.extra_srcs
    dep_info = gather_dep_info(ctx, ctx.attr.deps)

    # Note [Plugin order]
    plugin_decl = reversed(ctx.attr.plugins)
    plugin_dep_info = gather_dep_info(
        ctx,
        [dep for plugin in plugin_decl for dep in plugin[GhcPluginInfo].deps],
    )
    plugins = [resolve_plugin_tools(ctx, plugin[GhcPluginInfo]) for plugin in plugin_decl]
    (preprocessors_inputs, preprocessors_input_manifests) = ctx.resolve_tools(tools = ctx.attr.tools)

    # Determine outputs
    with_profiling = is_profiling_enabled(hs)
    hs_boot = paths.split_extension(src.path)[1] in [".hs-boot", ".lhs-boot"]
    extension_template = "%s"
    if hs_boot:
        extension_template = extension_template + "-boot"
    if with_profiling:
        extension_template = "p_" + extension_template
    extension_template = "." + extension_template

    odir_raw = "_odir_" + hs.name
    output_path = paths.join(odir_raw, paths.relativize(src.path, paths.join(ctx.label.package, ctx.attr.src_strip_prefix)))
    obj = ctx.actions.declare_file(paths.replace_extension(output_path, extension_template % "o"))
    dyn_obj = ctx.actions.declare_file(paths.replace_extension(output_path, extension_template % "dyn_o"))
    interface = ctx.actions.declare_file(paths.replace_extension(output_path, extension_template % "hi"))
    dyn_interface = ctx.actions.declare_file(paths.replace_extension(output_path, extension_template % "dyn_hi"))

    # TODO[AH] Support additional outputs such as `.hie`.

    # Construct the import search paths for this module
    workspace_root = paths.join(ctx.bin_dir.path, ctx.label.workspace_root)
    package_root = paths.join(workspace_root, ctx.label.package)
    import_dir = paths.join(package_root, odir_raw)

    # Construct compiler arguments

    args = ctx.actions.args()
    # The -hidir must be specified with -i as well or weird behavior ensues.
    # Moreover ghc does not seem to pick up interface files from other -i directories
    # when hidir is used.
    args.add_all([
        "-c", src,
        "-hidir", paths.join(package_root, odir_raw),
        "-odir", paths.join(package_root, odir_raw),
        "-i" + paths.join(package_root, odir_raw),
    ])
    args.add_all([
        "-v0",
        "-fPIC",
        "-hide-all-packages",
        # Should never trigger in sandboxed builds, but can be useful
        # to debug issues in non-sandboxed builds.
        "-Wmissing-home-modules",
    ])
    # Needed for TH
    args.add("-dynamic-too")
    if with_profiling:
        args.add_all([
            "-prof",
            "-fexternal-interpreter",
            "-hisuf",
            "p_hi",
            "-osuf",
            "p_o",
        ])
    package_name = getattr(ctx.attr, "package_name", None)
    if not package_name:
        package_name = ctx.attr._package_name_setting[BuildSettingInfo].value
    if package_name != "":
        args.add_all([
            "-this-unit-id",
            package_name,
            "-optP-DCURRENT_PACKAGE_KEY=\"{}\"".format(package_name),
        ])
    if not hs.toolchain.is_windows:
        # A static GHC RTS requires -fPIC. However, on Unix we also require
        # -fexternal-dynamic-refs, otherwise GHC still generates R_X86_64_PC32
        # relocations which prevents loading these static libraries as PIC.
        args.add("-fexternal-dynamic-refs")

    # GHC expects the CC compiler as the assembler, but segregates the
    # set of flags to pass to it when used as an assembler. So we have
    # to set both -optc and -opta.
    args.add_all(cc.compiler_flags, format_each = "-optc%s")
    args.add_all(cc.compiler_flags, format_each = "-opta%s")

    # Write the -optP flags to a parameter file because they can be very long on Windows
    # e.g. 27Kb for grpc-haskell
    optp_args_file = hs.actions.declare_file("optp_args_%s" % hs.name)
    optp_args = hs.actions.args()
    optp_args.add_all(cc.cpp_flags)
    optp_args.set_param_file_format("multiline")
    hs.actions.write(optp_args_file, optp_args)
    args.add(optp_args_file, format = "-optP@%s")

    args.add_all(cc.include_args)

    transitive_import_dirs = depset(
        direct = [dep[HaskellModuleInfo].import_dir for dep in ctx.attr.deps if HaskellModuleInfo in dep],
        transitive = [dep[HaskellModuleInfo].transitive_import_dirs for dep in ctx.attr.deps if HaskellModuleInfo in dep],
    )

    # Collect module dependency arguments
    # TODO[AH] Include object search paths for template Haskell dependencies.
    #   See https://github.com/tweag/rules_haskell/issues/1382

    # Collect library dependency arguments
    (pkg_info_inputs, pkg_info_args) = pkg_info_to_compile_flags(
        hs,
        pkg_info = expose_packages(
            package_ids = hs.package_ids,
            package_databases = dep_info.package_databases,
            # TODO[AH] Support version macros
            version = None,
        ),
        plugin_pkg_info = expose_packages(
            package_ids = [
                pkg_id
                for plugin in plugins
                for pkg_id in all_dependencies_package_ids(plugin.deps)
            ],
            package_databases = plugin_dep_info.package_databases,
            version = None,
        ),
        prefix = "compile-",
    )
    args.add_all(pkg_info_args)

    for plugin in plugins:
        args.add("-fplugin={}".format(plugin.module))
        for opt in plugin.args:
            args.add_all(["-fplugin-opt", "{}:{}".format(plugin.module, opt)])

    plugin_tool_inputs = depset(transitive = [plugin.tool_inputs for plugin in plugins])
    plugin_tool_input_manifests = [
        manifest
        for plugin in plugins
        for manifest in plugin.tool_input_manifests
    ]

    # TODO[AH] Support package id - see `-this-unit-id` flag.

    args.add_all(hs.toolchain.ghcopts)

    args.add_all(_expand_make_variables("ghcopts", ctx, ctx.attr.ghcopts))

    # Transitive library dependencies for runtime.
    link_libraries(
        get_ghci_library_files(hs, cc.cc_libraries_info, cc.cc_libraries, for_th_only = True),
        args,
    )

    transitive_interface_files = depset(
        direct = [
            (dep[HaskellModuleInfo].import_dir, dep[HaskellModuleInfo].interface_file) for dep in ctx.attr.deps if HaskellModuleInfo in dep
        ] + [
            (dep[HaskellModuleInfo].import_dir, dep[HaskellModuleInfo].dyn_interface_file) for dep in ctx.attr.deps if HaskellModuleInfo in dep
        ],
        transitive = [dep[HaskellModuleInfo].transitive_interface_files for dep in ctx.attr.deps if HaskellModuleInfo in dep],
    )

    transitive_object_files = depset(
        direct = [
            (dep[HaskellModuleInfo].import_dir, dep[HaskellModuleInfo].dyn_object_file) for dep in ctx.attr.deps if HaskellModuleInfo in dep
        ] + [
            (dep[HaskellModuleInfo].import_dir, dep[HaskellModuleInfo].object_file) for dep in ctx.attr.deps if HaskellModuleInfo in dep
        ],
        transitive = [dep[HaskellModuleInfo].transitive_object_files for dep in ctx.attr.deps if HaskellModuleInfo in dep],
    )

    module_interface_files = depset(module_files_for_build(hs, transitive_interface_files.to_list(), odir_raw))
    module_object_files = depset(module_files_for_build(hs, transitive_object_files.to_list(), odir_raw))

    # Compile the module
    hs.toolchain.actions.run_ghc(
        hs,
        cc,
        inputs = depset(
            direct = [src] + extra_srcs + [optp_args_file],
            transitive = [
                dep_info.package_databases,
                dep_info.interface_dirs,
                pkg_info_inputs,
                plugin_dep_info.package_databases,
                plugin_dep_info.interface_dirs,
                plugin_dep_info.hs_libraries,
                plugin_tool_inputs,
                preprocessors_inputs,
                module_interface_files,
                module_object_files,
                dep_info.hs_libraries,
                depset(get_ghci_library_files(hs, cc.cc_libraries_info, cc.transitive_libraries + cc.plugin_libraries)),
            ],
        ),
        input_manifests = preprocessors_input_manifests + plugin_tool_input_manifests,
        outputs = [obj, dyn_obj, interface, dyn_interface],
        mnemonic = "HaskellBuildObject" + ("Prof" if with_profiling else ""),
        progress_message = "HaskellBuildObject {}".format(hs.label),
        env = hs.env,
        arguments = args,
    )

    # Construct and return providers

    default_info = DefaultInfo(
        files = depset(direct = [obj, dyn_obj, interface, dyn_interface]),
    )
    module_info = HaskellModuleInfo(
        object_file = obj,
        dyn_object_file = dyn_obj,
        import_dir = import_dir,
        interface_file = interface,
        dyn_interface_file = dyn_interface,
        transitive_object_files = transitive_object_files,
        transitive_interface_files = transitive_interface_files,
        transitive_import_dirs = transitive_import_dirs,
    )

    return [default_info, module_info]
