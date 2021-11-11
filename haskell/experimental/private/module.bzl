load("@bazel_skylib//lib:paths.bzl", "paths")
load(
    "//haskell:private/dependencies.bzl",
    "gather_dep_info",
)
load(
    "//haskell:private/expansions.bzl",
    "expand_make_variables",
    "haskell_library_extra_label_attrs",
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

def _build_haskell_module(ctx, hs, cc, posix, dep_info, package_name, with_shared, hidir, odir, module_outputs, interface_inputs, object_inputs, module):
    """Build a module

    Args:
      ctx: The context of the binary, library, or test rule using the module
      hs: Haskell context
      cc: CcInteropInfo, information about C dependencies.
      posix: posix toolchain
      with_shared: Whether to build dynamic object files
      hidir: The directory in which to output interface files
      odir: The directory in which to output object files
      module_outputs: A struct containing the interfaces and object files produced for a haskell_module.
      interface_inputs: A depset containing the interface files needed as input
      object_inputs: A depset containing the object files needed as input
      module: The Target of the haskell_module rule
    """

    moduleAttr = module[HaskellModuleInfo].attr

    # Collect dependencies
    src = moduleAttr.src.files.to_list()[0]
    extra_srcs = [f for t in moduleAttr.extra_srcs + ctx.attr.extra_srcs for f in t.files.to_list()]

    # Note [Plugin order]
    plugin_decl = reversed(ctx.attr.plugins + moduleAttr.plugins)
    plugin_dep_info = gather_dep_info(
        moduleAttr.name,
        [dep for plugin in plugin_decl for dep in plugin[GhcPluginInfo].deps],
    )
    plugins = [resolve_plugin_tools(ctx, plugin[GhcPluginInfo]) for plugin in plugin_decl]
    (preprocessors_inputs, preprocessors_input_manifests) = ctx.resolve_tools(tools = ctx.attr.tools + moduleAttr.tools)

    # Determine outputs
    with_profiling = is_profiling_enabled(hs)

    # TODO[AH] Support additional outputs such as `.hie`.

    # Construct compiler arguments

    args = ctx.actions.args()
    args.add_all([
        "-c",
        "-odir",
        paths.join(hs.bin_dir.path, hs.package_root, odir),
        "-hidir",
        paths.join(hs.bin_dir.path, hs.package_root, hidir),
        "-i" + paths.join(hs.bin_dir.path, hs.package_root, hidir),
        src,
    ])
    if with_shared:
        args.add("-dynamic-too")
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
        prefix = "compile-{}-".format(module.label.name),
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

    args.add_all(expand_make_variables("ghcopts", ctx, ctx.attr.ghcopts, haskell_library_extra_label_attrs(ctx.attr)))
    module_extra_attrs = [
        [moduleAttr.src],
        moduleAttr.extra_srcs,
        moduleAttr.plugins,
        moduleAttr.tools,
    ]
    args.add_all(expand_make_variables("ghcopts", ctx, moduleAttr.ghcopts, module_extra_attrs))

    outputs = [module_outputs.hi]
    if module_outputs.o:
        outputs += [module_outputs.o]
    if with_shared:
        outputs += [module_outputs.dyn_hi]
        if module_outputs.dyn_o:
            outputs += [module_outputs.dyn_o]

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
                interface_inputs,
                object_inputs,
            ],
        ),
        input_manifests = preprocessors_input_manifests + plugin_tool_input_manifests,
        outputs = outputs,
        mnemonic = "HaskellBuildObject" + ("Prof" if with_profiling else ""),
        progress_message = "HaskellBuildObject {} {}".format(hs.label, module.label),
        env = hs.env,
        arguments = args,
        extra_name = module.label.package.replace("/", "_") + "_" + module.label.name,
    )

def get_module_path_from_target(module):
    src = module[HaskellModuleInfo].attr.src.files.to_list()[0].path
    src_strip_prefix = module[HaskellModuleInfo].attr.src_strip_prefix
    workspace_root = module.label.workspace_root

    if src_strip_prefix.startswith("/"):
        prefix_path = paths.join(workspace_root, src_strip_prefix[1:])
    else:
        prefix_path = paths.join(workspace_root, module.label.package, src_strip_prefix)

    return paths.relativize(src, prefix_path)

def _declare_module_outputs(hs, with_shared, hidir, odir, module):
    module_path = get_module_path_from_target(module)

    hs_boot = paths.split_extension(module_path)[1] in [".hs-boot", ".lhs-boot"]
    with_profiling = is_profiling_enabled(hs)
    extension_template = "%s"
    if hs_boot:
        extension_template = extension_template + "-boot"
    if with_profiling:
        extension_template = "p_" + extension_template
    extension_template = "." + extension_template

    hi = hs.actions.declare_file(paths.join(hidir, paths.replace_extension(module_path, extension_template % "hi")))
    o = None if hs_boot else hs.actions.declare_file(paths.join(odir, paths.replace_extension(module_path, extension_template % "o")))
    if with_shared:
        dyn_o = None if hs_boot else hs.actions.declare_file(paths.join(odir, paths.replace_extension(module_path, extension_template % "dyn_o")))
        dyn_hi = hs.actions.declare_file(paths.join(hidir, paths.replace_extension(module_path, extension_template % "dyn_hi")))
    else:
        dyn_hi = None
        dyn_o = None
    return struct(hi = hi, dyn_hi = dyn_hi, o = o, dyn_o = dyn_o)

def _collect_module_outputs_of_direct_deps(with_shared, module_outputs, dep):
    his = [
        module_outputs[m.label].hi
        for m in dep[HaskellModuleInfo].direct_module_deps
    ]
    os = [
        o
        for m in dep[HaskellModuleInfo].direct_module_deps
        for o in [module_outputs[m.label].o]
        if o  # boot module files produce no useful object files
    ]
    if with_shared:
        his += [
            module_outputs[m.label].dyn_hi
            for m in dep[HaskellModuleInfo].direct_module_deps
        ]
        os += [
            dyn_o
            for m in dep[HaskellModuleInfo].direct_module_deps
            for dyn_o in [module_outputs[m.label].dyn_o]
            if dyn_o  # boot module files produce no useful object files
        ]
    return his, os

def _collect_module_inputs(module_input_map, directs, dep):
    """ Put together inputs coming from direct and transitive dependencies.

    Args:
      module_input_map: maps labels of dependencies to all the inputs they require
      directs: inputs of direct depedencies
      dep: the target for which to collect inputs

    Returns:
      A depset with all of the inputs for the given target
    """
    all_inputs = depset(
        direct = directs,
        transitive = [
            module_input_map[m.label]  # Will be set by a previous iteration, since all deps were visited before.
            for m in dep[HaskellModuleInfo].direct_module_deps
        ],
    )
    module_input_map[dep.label] = all_inputs
    return all_inputs

def _reorder_module_deps_to_postorder(label, modules):
    """ Reorders modules to a postorder traversal of the dependency dag.

    Args:
      label: The label of the rule with the modules attribute
      modules: The modules comming from a modules attribute. This list must
               be the transitive closure of all the module dependencies.
    """
    transitive_module_dep_labels = depset(
        direct = [m.label for m in modules],
        transitive = [m[HaskellModuleInfo].transitive_module_dep_labels for m in modules],
    ).to_list()
    module_map = {m.label: m for m in modules}
    if len(module_map) != len(transitive_module_dep_labels):
        diff = ", ".join([str(x) for x in transitive_module_dep_labels if not x in module_map])
        fail("There are modules missing in the modules attribute of {0}: {1}".format(label, diff))
    return [module_map[lbl] for lbl in transitive_module_dep_labels]

def build_haskell_modules(ctx, hs, cc, posix, package_name, with_shared, hidir, odir):
    """ Build all the modules of haskell_module rules in ctx.attr.modules
        and in their dependencies

    Args:
      ctx: The context of the rule with module dependencies
      hs: Haskell context
      cc: CcInteropInfo, information about C dependencies
      posix: posix toolchain
      with_shared: Whether to build dynamic object files
      hidir: The directory in which to output interface files
      odir: The directory in which to output object files

    Returns:
      struct(his, dyn_his, os, dyn_os): each component containts the interface, dynamic interface, object,
      and dynamic object files of all transitive module dependencies.
    """

    dep_info = gather_dep_info(ctx.attr.name, ctx.attr.deps)
    transitive_module_deps = _reorder_module_deps_to_postorder(ctx.label, ctx.attr.modules)
    module_outputs = {dep.label: _declare_module_outputs(hs, with_shared, hidir, odir, dep) for dep in transitive_module_deps}

    module_interfaces = {}
    module_objects = {}
    for dep in transitive_module_deps:
        his, os = _collect_module_outputs_of_direct_deps(with_shared, module_outputs, dep)
        interface_inputs = _collect_module_inputs(module_interfaces, his, dep)
        object_inputs = _collect_module_inputs(module_objects, os, dep)

        _build_haskell_module(ctx, hs, cc, posix, dep_info, package_name, with_shared, hidir, odir, module_outputs[dep.label], interface_inputs, object_inputs, dep)

    module_outputs_list = module_outputs.values()
    hi_set = depset([outputs.hi for outputs in module_outputs_list])
    o_set = depset([outputs.o for outputs in module_outputs_list if outputs.o])
    if with_shared:
        dyn_hi_set = depset([outputs.dyn_hi for outputs in module_outputs_list])
        dyn_o_set = depset([outputs.dyn_o for outputs in module_outputs_list if outputs.dyn_o])
    else:
        dyn_hi_set = depset()
        dyn_o_set = depset()

    return struct(
        his = hi_set,
        dyn_his = dyn_hi_set,
        os = o_set,
        dyn_os = dyn_o_set,
    )

def haskell_module_impl(ctx):
    module_deps = [dep for dep in ctx.attr.deps if HaskellModuleInfo in dep]
    transitive_module_dep_labels = depset(
        direct = [dep.label for dep in module_deps],
        transitive = [dep[HaskellModuleInfo].transitive_module_dep_labels for dep in module_deps],
        order = "postorder",
    )
    return [
        DefaultInfo(),
        HaskellModuleInfo(
            attr = ctx.attr,
            direct_module_deps = module_deps,
            transitive_module_dep_labels = transitive_module_dep_labels,
        ),
    ]
