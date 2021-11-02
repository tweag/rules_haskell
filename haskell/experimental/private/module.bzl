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
    "//haskell:private/dependencies.bzl",
    "gather_dep_info",
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

def _expand_make_variables(name, ctx, moduleAttr, strings):
    # All labels in all attributes should be location-expandable.
    extra_label_attrs = [
        [ctx.attr.src],
        ctx.attr.extra_srcs,
        ctx.attr.plugins,
        ctx.attr.tools,
        [moduleAttr.src],
        moduleAttr.extra_srcs,
        moduleAttr.plugins,
        moduleAttr.tools,
    ]
    return expand_make_variables(name, ctx, strings, extra_label_attrs)

def _build_haskell_module(ctx, moduleAttr, hs, cc, posix, package_name, hidir, odir, module_interface_files, module_object_files):
    """Build a module

    Args:
      ctx: The context of the binary, library, or test rule using the module
      moduleAttr: The attributes of the haskell_module rule
      hs: Haskell context
      cc: cc context
      posix: posix context
      hidir: The directory in which to output interface files
      odir: The directory in which to output object files
      module_interface_files: The list of interface files produced by all the haskell_module dependencies
      module_object_files: The list of object files produced by all the haskell_module dependencies

    Returns:
      ([File], [File]): a pair containing a list produced interface files and a list of produced object files
    """

    # Collect dependencies
    src = moduleAttr.src.files.to_list()[0]
    src_strip_prefix = moduleAttr.src_strip_prefix
    extra_srcs = [f for t in moduleAttr.extra_srcs + ctx.attr.extra_srcs for f in t.files]
    dep_info = gather_dep_info(moduleAttr.name, moduleAttr.deps)

    # Note [Plugin order]
    plugin_decl = reversed(moduleAttr.plugins)
    plugin_dep_info = gather_dep_info(
        moduleAttr.name,
        [dep for plugin in plugin_decl for dep in plugin[GhcPluginInfo].deps],
    )
    plugins = [resolve_plugin_tools(ctx, plugin[GhcPluginInfo]) for plugin in plugin_decl]
    (preprocessors_inputs, preprocessors_input_manifests) = ctx.resolve_tools(tools = ctx.attr.tools + moduleAttr.tools)

    # Determine outputs
    workspace_root = paths.join(ctx.bin_dir.path, ctx.label.workspace_root)
    package_root = paths.join(workspace_root, ctx.label.package)
    if src_strip_prefix.startswith("/"):
        import_dir = paths.join(workspace_root, src_strip_prefix[1:])
    else:
        import_dir = paths.join(package_root, src_strip_prefix)
    module_path = paths.relativize(src.path, import_dir)

    with_profiling = is_profiling_enabled(hs)
    hs_boot = paths.split_extension(src.path)[1] in [".hs-boot", ".lhs-boot"]
    extension_template = "%s"
    if hs_boot:
        extension_template = extension_template + "-boot"
    if with_profiling:
        extension_template = "p_" + extension_template
    extension_template = "." + extension_template

    obj = ctx.actions.declare_file(paths.join(odir.path, paths.replace_extension(module_path, extension_template % "o")))
    interface = ctx.actions.declare_file(paths.join(hidir.path, paths.replace_extension(module_path, extension_template % "hi")))

    # TODO[AH] Support additional outputs such as `.hie`.

    # Construct compiler arguments

    args = ctx.actions.args()
    args.add_all(["-c", "-odir", odir, "-hidir", hidir, src])
    args.add_all([
        "-v0",
        "-fPIC",
        "-hide-all-packages",
        # Should never trigger in sandboxed builds, but can be useful
        # to debug issues in non-sandboxed builds.
        "-Wmissing-home-modules",
    ])
    if with_profiling:
        args.add_all([
            "-prof",
            "-fexternal-interpreter",
            "-hisuf",
            "p_hi",
            "-osuf",
            "p_o",
        ])
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

    args.add_all(_expand_make_variables("ghcopts", ctx, moduleAttr, ctx.attr.ghcopts + moduleAttr.ghcopts))

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
                # TODO[AH] Factor this out
                # TODO[AH] Include object files for template Haskell dependencies.
                module_interface_files,
            ],
        ),
        input_manifests = preprocessors_input_manifests + plugin_tool_input_manifests,
        outputs = [obj, interface],
        mnemonic = "HaskellBuildObject" + ("Prof" if with_profiling else ""),
        progress_message = "HaskellBuildObject {}".format(hs.label),
        env = hs.env,
        arguments = args,
    )

    # Return produced files
    return [interface], [obj]

def _build_haskell_modules(ctx, hs, cc, posix, package_name, hidir, odir, module_outputs, module_dep):
    # This is an encoding of a recursive function, since bazel doesn't
    # allow recursion.

    # The stack holds the module target being processed and the list of
    # results collected from its dependencies so far.
    module_stack = [(module_dep, [])]
    # The loop breaks when the stack is empty
    for i in range(0, 0x7fffffff):
        dep, children_results = module_stack[len(module_stack) - 1]
        if len(children_results) == 0 and (dep.label in module_outputs or not HaskellModuleInfo in dep):
            # Don't recurse if the result has been already computed
            # or dependency is not from a haskell_module rule

            if HaskellModuleInfo in dep:
                outputs = module_outputs[dep.label]
            else:
                outputs = (depset(), depset())

            module_stack.pop()
            if len(module_stack) == 0:
                return outputs

            # Add the result to the result list of the parent
            _, children_results = module_stack[len(module_stack) - 1]
            children_results.append(outputs)

        elif len(children_results) < len(dep[HaskellModuleInfo].attr.deps):
            # Create the action for the next child dependency
            module_stack.append((dep[HaskellModuleInfo].attr.deps[len(children_results)], []))

        else:
            # All actions for all the children have been created
            # Now create the action for the current target. 
            transitive_interfaces = [interface_set for (interface_set, _) in children_results]
            transitive_objs = [obj_set for (_, obj_set) in children_results]

            interfaces, objs = _build_haskell_module(
                ctx,
                dep[HaskellModuleInfo].attr,
                hs,
                cc,
                posix,
                package_name,
                hidir,
                odir,
                depset(transitive = transitive_interfaces),
                depset(transitive = transitive_objs),
            )

            outputs = (
                depset(direct = interfaces, transitive = transitive_interfaces),
                depset(direct = objs, transitive = transitive_objs),
            )
            module_outputs[dep.label] = outputs

            module_stack.pop()
            if len(module_stack) == 0:
                return outputs

            # Add the result to the result list of the parent
            _, children_results = module_stack[len(module_stack) - 1]
            children_results.append(outputs)


def build_haskell_modules(ctx, hs, cc, posix, package_name, hidir, odir):
    """ Build all the modules of haskell_module rules in ctx.attr.modules
        and in their dependencies

    Args:
      ctx: The context of the rule with module dependencies
      hs: Haskell context
      cc: cc context
      posix: posix context
      hidir: The directory in which to output interface files
      odir: The directory in which to output object files

    Returns:
      (depset(File), depset(File)): a pair containing a depset of the interface
      files of all transitive module dependencies and similarly a depset of all
      the object files
    """

    module_outputs = {}
    transitive_interfaces = []
    transitive_objs = []
    for m in ctx.attr.modules:
        interface_set, obj_set = _build_haskell_modules(ctx, hs, cc, posix, package_name, hidir, odir, module_outputs, m)
        transitive_interfaces.append(interface_set)
        transitive_objs.append(obj_set)

    return depset(transitive = transitive_interfaces), depset(transitive = transitive_objs)

def haskell_module_impl(ctx):
    return [DefaultInfo(), HaskellModuleInfo(attr = ctx.attr)]
