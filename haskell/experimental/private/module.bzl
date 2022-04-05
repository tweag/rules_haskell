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
load("//haskell:private/pkg_id.bzl", "pkg_id")
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
load("//haskell:providers.bzl", "HaskellInfo", "HaskellLibraryInfo")

# Note [Narrowed Dependencies]
#
# Usually, when a module M depends on a library L, it doesn't depend on
# all the modules of the library. The user expresses which modules are
# needed with the cross_library_deps attribute and rules_haskell looks
# for the modules in the libraries of the narrowed_deps attribute in the
# enclosing haskell_library.
#
# build_haskell_modules from module.bzl produces dictionaries that say
# for every module label which interface and object files it produces
# and which interface and object files it depends upon transitively.
# These dictionaries end up in the HaskellInfo provider.
#
# It is not strictly necessary to depend on all interface and object
# files transitively, but we have no easy way to discern which files
# from the transitive dependencies are actually needed, so we
# over-approximate by depending on all of them.
#
# When compilation doesn't involve Template Haskell, a module only needs
# the interface files of its module dependencies. When the user
# specifies that a module uses Template Haskell via the enable_th
# attribute, we also pass the potentially needed object files in the
# inputs of the build action of haskell_module.
#
# Telling ghc which interface files are available is easy by specifying
# package databases and package-ids on the command line. Passing object
# files is harder because ghc only expects the object files of libraries
# to be linked together in installed packages. It is possible to tell to
# ghc about individual object files by listing their filepaths on the
# command line, but see Note [Empty Libraries] in haskell_impl.bzl for an
# extra nuance.
#
# Unfortunately, passing object files in the command line doesn't cause
# ghc to load them in the external interpreter, so narrowing doesn't
# work in any configuration needing the external interpreter. Therefore,
# profiling builds use the libraries of narrowed_deps instead of the
# their object files.

# Note [Deps as both narrowed and not narrowed]
#
# Package databases are searched in reversed order with respect to
# how they appear in the command line of GHC or in environment files.
#
# If a package appears both in narrowed_deps and in normal deps, different
# versions of the package will appear in different package databases.
# One of the versions points to an empty shared library, and the other
# version points to the real shared library.
#
# When both versions are needed, we want package database pointing
# to the real shared library to take precedence. Thus it should appear
# last in the command line or the environment files.

def _build_haskell_module(
        ctx,
        hs,
        cc,
        posix,
        dep_info,
        narrowed_deps_info,
        package_name,
        with_profiling,
        with_shared,
        enable_th,
        hidir,
        odir,
        module_outputs,
        interface_inputs,
        object_inputs,
        narrowed_objects,
        extra_ldflags_file,
        module):
    """Build a module

    Args:
      ctx: The context of the binary, library, or test rule using the module
      hs: Haskell context
      cc: CcInteropInfo, information about C dependencies.
      posix: posix toolchain
      dep_info: info on dependencies in deps
      narrowed_deps_info: info on dependencies in narrowed_deps
      package_name: name of this package, or empty if building a binary
      with_profiling: Whether to build profiling object files
      with_shared: Whether to build dynamic object files
      enable_th: Whether object files and shared libraries need to be exposed to the build action
      hidir: The directory in which to output interface files
      odir: The directory in which to output object files
      module_outputs: A struct containing the interfaces and object files produced for a haskell_module.
      interface_inputs: A depset containing the interface files needed as input
      object_inputs: A depset containing the object files needed as input
      narrowed_objects: A depset containing the narrowed object files needed as arguments to ghc.
      extra_ldflags_file: A File with flags for ld or None.
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
    if hs.mode == "opt":
        args.add("-O2")
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

    if plugins or enable_th:
        # cc toolchain linker flags would be necessary when the interpreter wants to
        # load any libraries
        args.add_all(cc.linker_flags, format_each = "-optl%s")

    # Collect library dependency arguments
    (pkg_info_inputs, pkg_info_args) = pkg_info_to_compile_flags(
        hs,
        pkg_info = expose_packages(
            package_ids = hs.package_ids,
            package_databases = depset(
                transitive = [
                    # Mind the order in which databases are specified here.
                    # See Note [Deps as both narrowed and not narrowed].
                    narrowed_deps_info.empty_lib_package_databases,
                    dep_info.package_databases,
                ],
                order = "preorder",
            ),
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

    args.add_all(hs.toolchain.ghcopts)

    args.add_all(expand_make_variables("ghcopts", ctx, ctx.attr.ghcopts, haskell_library_extra_label_attrs(ctx.attr)))
    module_extra_attrs = [
        [moduleAttr.src],
        moduleAttr.extra_srcs,
        moduleAttr.plugins,
        moduleAttr.tools,
    ]
    if plugins and not enable_th:
        # For #1681. These suppresses bogus warnings about missing libraries which
        # aren't really needed.
        args.add("-Wno-missed-extra-shared-lib")
    args.add_all(expand_make_variables("ghcopts", ctx, moduleAttr.ghcopts, module_extra_attrs))
    if enable_th:
        args.add_all(narrowed_objects)

    outputs = [module_outputs.hi]
    if module_outputs.o:
        outputs += [module_outputs.o]
    if with_shared:
        outputs += [module_outputs.dyn_hi]
        if module_outputs.dyn_o:
            outputs += [module_outputs.dyn_o]

    input_files = [src] + extra_srcs + [optp_args_file]
    if enable_th and extra_ldflags_file:
        args.add("-optl@{}".format(extra_ldflags_file.path))
        input_files.append(extra_ldflags_file)

    # Compile the module
    hs.toolchain.actions.run_ghc(
        hs,
        cc,
        inputs = depset(
            direct = input_files,
            transitive = [
                dep_info.package_databases,
                dep_info.interface_dirs,
                narrowed_deps_info.empty_lib_package_databases,
                narrowed_deps_info.deps_interface_dirs,
                pkg_info_inputs,
                plugin_dep_info.package_databases,
                plugin_dep_info.interface_dirs,
                plugin_dep_info.hs_libraries,
                plugin_tool_inputs,
                preprocessors_inputs,
                interface_inputs,
            ] + [
                files
                for files in [
                    dep_info.hs_libraries,
                    dep_info.deps_hs_libraries,
                    narrowed_deps_info.deps_hs_libraries,
                    narrowed_deps_info.empty_hs_libraries,
                    object_inputs,
                ]
                # libraries and object inputs are only needed if the module uses TH
                if enable_th
            ],
        ),
        input_manifests = preprocessors_input_manifests + plugin_tool_input_manifests,
        outputs = outputs,
        mnemonic = "HaskellBuildObject" + ("Prof" if with_profiling else ""),
        progress_message = "HaskellBuildObject {} {}".format(hs.label, module.label),
        env = hs.env,
        arguments = args,
        extra_name = module.label.package.replace("/", "_") + "_" + module.label.name,
        worker = ctx.executable.haskell_module_worker,
    )

def get_module_path_from_target(module):
    module_name = module[HaskellModuleInfo].attr.module_name
    if module_name:
        return module_name.replace(".", "/")

    src = module[HaskellModuleInfo].attr.src.files.to_list()[0].path
    src_strip_prefix = module[HaskellModuleInfo].attr.src_strip_prefix
    workspace_root = module.label.workspace_root

    if src_strip_prefix.startswith("/"):
        prefix_path = paths.join(workspace_root, src_strip_prefix[1:])
    else:
        prefix_path = paths.join(workspace_root, module.label.package, src_strip_prefix)

    return paths.split_extension(paths.relativize(src, prefix_path))[0]

def _declare_module_outputs(hs, with_profiling, with_shared, hidir, odir, module):
    module_path = get_module_path_from_target(module)

    src = module[HaskellModuleInfo].attr.src.files.to_list()[0].path
    hs_boot = paths.split_extension(src)[1] in [".hs-boot", ".lhs-boot"]
    extension_template = "%s"
    if hs_boot:
        extension_template = extension_template + "-boot"
    if with_profiling:
        extension_template = "p_" + extension_template
    extension_template = module_path + "." + extension_template

    hi = hs.actions.declare_file(paths.join(hidir, extension_template % "hi"))
    o = None if hs_boot else hs.actions.declare_file(paths.join(odir, extension_template % "o"))
    if with_shared:
        dyn_o = None if hs_boot else hs.actions.declare_file(paths.join(odir, extension_template % "dyn_o"))
        dyn_hi = hs.actions.declare_file(paths.join(hidir, extension_template % "dyn_hi"))
    else:
        dyn_hi = None
        dyn_o = None
    return struct(hi = hi, dyn_hi = dyn_hi, o = o, dyn_o = dyn_o)

def _collect_module_outputs_of_direct_deps(with_shared, module_outputs, dep):
    his = [
        module_outputs[m.label].hi
        for m in dep[HaskellModuleInfo].direct_module_deps
        if m.label in module_outputs
    ]
    os = [
        o
        for m in dep[HaskellModuleInfo].direct_module_deps
        if m.label in module_outputs
        for o in [module_outputs[m.label].o]
        if o  # boot module files produce no useful object files
    ]
    if with_shared:
        his += [
            module_outputs[m.label].dyn_hi
            for m in dep[HaskellModuleInfo].direct_module_deps
            if m.label in module_outputs
        ]
        dyn_os = [
            dyn_o
            for m in dep[HaskellModuleInfo].direct_module_deps
            if m.label in module_outputs
            for dyn_o in [module_outputs[m.label].dyn_o]
            if dyn_o  # boot module files produce no useful object files
        ]
    else:
        dyn_os = []
    return his, os, dyn_os

def _collect_module_inputs(module_input_map, extra_inputs, directs, dep):
    """ Put together inputs coming from direct and transitive dependencies.

    Args:
      module_input_map: maps labels of dependencies to all the inputs they require
      extra_inputs: an addition depset of inputs to include
      directs: inputs of direct dependencies
      dep: the target for which to collect inputs

    Returns:
      A depset with all of the inputs for the given target
    """
    all_inputs = depset(
        direct = directs,
        transitive = [extra_inputs] + [
            module_input_map[m.label]  # Will be set by a previous iteration, since all deps were visited before.
            for m in dep[HaskellModuleInfo].direct_module_deps
            if m.label in module_input_map
        ],
    )
    module_input_map[dep.label] = all_inputs
    return all_inputs

def _collect_narrowed_deps_module_files(ctx_label, per_module_transitive_files, dep):
    direct_cross_library_deps = dep[HaskellModuleInfo].direct_cross_library_deps
    transitives = [
        per_module_transitive_files[m.label]
        for m in direct_cross_library_deps
        if m.label in per_module_transitive_files
    ]
    if len(transitives) < len(direct_cross_library_deps):
        missing = [str(m.label) for m in direct_cross_library_deps if not m.label in per_module_transitive_files]
        fail("The following dependencies of {} can't be found in 'narrowed_deps' of {}: {}".format(
            dep.label,
            ctx_label,
            ", ".join(missing),
        ))

    return depset(transitive = transitives)

def _reorder_module_deps_to_postorder(label, modules):
    """ Reorders modules to a postorder traversal of the dependency dag.

    Args:
      label: The label of the rule with the modules attribute
      modules: The modules coming from a modules attribute. This list must
               contain the transitive closure of all the module dependencies
               in the enclosing library/binary/test.

    Returns:
      A list with the modules in postorder
    """
    transitive_module_dep_labels = depset(
        direct = [m.label for m in modules],
        transitive = [m[HaskellModuleInfo].transitive_module_dep_labels for m in modules],
        order = "postorder",
    ).to_list()
    module_map = {m.label: m for m in modules}
    if len(module_map) != len(transitive_module_dep_labels):
        missing = [str(x) for x in transitive_module_dep_labels if not x in module_map]
        fail("There are modules missing in the modules attribute of {0}: {1}".format(label, ", ".join(missing)))
    return [module_map[lbl] for lbl in transitive_module_dep_labels if lbl in module_map]

def _merge_depset_dicts(d0, d1):
    """Merges into dict d0 the depsets in dict d1"""
    for k, s in d1.items():
        s1 = d0.get(k)
        if s1:
            d0[k] = depset(transitive = [s1, s])
        else:
            d0[k] = s

def _merge_narrowed_deps_dicts(rule_label, narrowed_deps):
    """Merge the module files corresponding to library dependencies that can be narrowed

    Args:
      rule_label: The label of the rule with the modules attribute
      narrowed_deps: The contents of the narrowed_deps attribute

    Returns:
      struct(transitive_interfaces, transitive_objects, transitive_dyn_objects):
        transitive_interfaces: dict of module labels to their
           interfaces and the interfaces of their transitive module dependencies
        transitive_objects: dict of module labels to their
           object files and the object file of their transitive module
           dependencies
        transitive_dyn_objects: like per_module_transitive_objects but for dyn_o files
    """
    transitive_interfaces = {}
    transitive_objects = {}
    transitive_dyn_objects = {}
    for dep in narrowed_deps:
        if not HaskellInfo in dep or not HaskellLibraryInfo in dep:
            fail("{}: depedency {} is not a haskell_library as required when used in narrowed_deps".format(
                str(rule_label),
                str(dep.label),
            ))
        lib_info = dep[HaskellInfo]
        if not hasattr(lib_info, "per_module_transitive_interfaces") or not lib_info.per_module_transitive_interfaces:
            fail("""{}: haskell_library {} doesn't use the "modules" attribute as required when used in narrowed_deps""".format(
                str(rule_label),
                str(dep.label),
            ))
        _merge_depset_dicts(transitive_interfaces, lib_info.per_module_transitive_interfaces)
        _merge_depset_dicts(transitive_objects, lib_info.per_module_transitive_objects)
        _merge_depset_dicts(transitive_dyn_objects, lib_info.per_module_transitive_dyn_objects)
    return struct(
        transitive_interfaces = transitive_interfaces,
        transitive_objects = transitive_objects,
        transitive_dyn_objects = transitive_dyn_objects,
    )

def interfaces_as_list(with_shared, o):
    if with_shared:
        return [o.hi, o.dyn_hi]
    else:
        return [o.hi]

def build_haskell_modules(
        ctx,
        hs,
        cc,
        posix,
        package_name,
        with_profiling,
        with_shared,
        extra_ldflags_file,
        hidir,
        odir):
    """ Build all the modules of haskell_module rules in ctx.attr.modules
        and in their dependencies

    Args:
      ctx: The context of the rule with module dependencies
      hs: Haskell context
      cc: CcInteropInfo, information about C dependencies
      posix: posix toolchain
      package_name: package name if building a library or empty if building a binary
      with_profiling: Whether to build profiling object files
      with_shared: Whether to build dynamic object files
      extra_ldflags_file: A File with flags for ld or None.
      hidir: The directory in which to output interface files
      odir: The directory in which to output object files

    Returns:
      struct(his, dyn_his, os, dyn_os, per_module_transitive_interfaces, per_module_transitive_objects, per_module_transitive_dyn_objects):
        his: interface files of all modules in ctx.attr.modules
        dyn_his: dynamic interface files of all modules in ctx.attr.modules
        os: object files of all modules in ctx.attr.modules
        dyn_os: dynamic object files of all modules in ctx.attr.modules
        per_module_transitive_interfaces: dict of module labels to their
            interfaces and the interfaces of their transitive module
            dependencies. See Note [Narrowed Dependencies].
        per_module_transitive_objects: dict of module labels to their
            object files and the object files of their transitive module
            dependencies. See Note [Narrowed Dependencies].
        per_module_transitive_dyn_objects: like per_module_transitive_objects but for dyn_o files
    """
    per_module_maps = _merge_narrowed_deps_dicts(ctx.label, ctx.attr.narrowed_deps)

    # We produce separate infos for narrowed_deps and deps because the module
    # files in dep_info are given as inputs to the build action, but the
    # modules files from narrowed_deps_info are only given when haskell_module
    # declares to depend on them.
    dep_info = gather_dep_info(ctx.attr.name, ctx.attr.deps)
    narrowed_deps_info = gather_dep_info(ctx.attr.name, ctx.attr.narrowed_deps)
    all_deps_info = gather_dep_info(ctx.attr.name, ctx.attr.deps + ctx.attr.narrowed_deps)
    empty_deps_info = gather_dep_info(ctx.attr.name, [])
    transitive_module_deps = _reorder_module_deps_to_postorder(ctx.label, ctx.attr.modules)
    module_outputs = {dep.label: _declare_module_outputs(hs, with_profiling, with_shared, hidir, odir, dep) for dep in transitive_module_deps}

    module_interfaces = {}
    module_objects = {}
    module_dyn_objects = {}
    for dep in transitive_module_deps:
        # called in all cases to validate cross_library_deps, although the output
        # might be ignored when disabling narrowing
        narrowed_interfaces = _collect_narrowed_deps_module_files(ctx.label, per_module_maps.transitive_interfaces, dep)
        enable_th = dep[HaskellModuleInfo].attr.enable_th

        # Narrowing doesn't work when using the external interpreter so we disable it here
        if enable_th and with_profiling:
            dep_info_i = all_deps_info
            narrowed_deps_info_i = empty_deps_info
            narrowed_interfaces = depset()
            narrowed_objects = depset()
        else:
            dep_info_i = dep_info
            narrowed_deps_info_i = narrowed_deps_info

            # even if TH is not enabled, we collect the narrowed_objects for building
            # other modules that import this one and that might use TH
            narrowed_objects = _collect_narrowed_deps_module_files(ctx.label, per_module_maps.transitive_objects, dep)

        his, os, dyn_os = _collect_module_outputs_of_direct_deps(with_shared, module_outputs, dep)
        interface_inputs = _collect_module_inputs(module_interfaces, narrowed_interfaces, his, dep)
        object_inputs = depset(transitive = [
            _collect_module_inputs(module_objects, narrowed_objects, os, dep),
            _collect_module_inputs(module_dyn_objects, narrowed_objects, dyn_os, dep),
        ])

        _build_haskell_module(
            ctx,
            hs,
            cc,
            posix,
            dep_info_i,
            narrowed_deps_info_i,
            package_name,
            with_profiling,
            with_shared,
            enable_th,
            hidir,
            odir,
            module_outputs[dep.label],
            interface_inputs,
            object_inputs,
            narrowed_objects,
            extra_ldflags_file,
            dep,
        )

    module_outputs_list = module_outputs.values()
    hi_set = depset([outputs.hi for outputs in module_outputs_list])
    o_set = depset([outputs.o for outputs in module_outputs_list if outputs.o])
    if with_shared:
        dyn_hi_set = depset([outputs.dyn_hi for outputs in module_outputs_list])
        dyn_o_set = depset([outputs.dyn_o for outputs in module_outputs_list if outputs.dyn_o])
    else:
        dyn_hi_set = depset()
        dyn_o_set = depset()

    # put outputs and inputs together for each module
    per_module_transitive_interfaces0 = {
        dep.label: depset(
            interfaces_as_list(with_shared, module_outputs[dep.label]),
            transitive = [module_interfaces[dep.label]],
        )
        for dep in transitive_module_deps
    }
    _merge_depset_dicts(per_module_transitive_interfaces0, per_module_maps.transitive_interfaces)
    per_module_transitive_objects0 = {
        dep.label: depset(
            [module_outputs[dep.label].o],
            transitive = [module_objects[dep.label]],
        )
        for dep in transitive_module_deps
    }
    per_module_transitive_dyn_objects0 = {
        dep.label: depset(
            [module_outputs[dep.label].dyn_o],
            transitive = [module_dyn_objects[dep.label]],
        )
        for dep in transitive_module_deps
    } if with_shared else {}
    _merge_depset_dicts(per_module_transitive_objects0, per_module_maps.transitive_objects)
    _merge_depset_dicts(per_module_transitive_dyn_objects0, per_module_maps.transitive_dyn_objects)

    return struct(
        his = hi_set,
        dyn_his = dyn_hi_set,
        os = o_set,
        dyn_os = dyn_o_set,
        per_module_transitive_interfaces = per_module_transitive_interfaces0,
        per_module_transitive_objects = per_module_transitive_objects0,
        per_module_transitive_dyn_objects = per_module_transitive_dyn_objects0,
    )

def haskell_module_impl(ctx):
    deps = ctx.attr.deps + ctx.attr.cross_library_deps
    nonmodules = [str(dep.label) for dep in deps if not HaskellModuleInfo in dep]
    if nonmodules:
        fail("The following dependencies of {} aren't defined with haskell_module: {}".format(ctx.label, ", ".join(nonmodules)))

    transitive_module_dep_labels = depset(
        direct = [dep.label for dep in ctx.attr.deps],
        transitive = [dep[HaskellModuleInfo].transitive_module_dep_labels for dep in ctx.attr.deps],
        order = "postorder",
    )
    return [
        DefaultInfo(),
        HaskellModuleInfo(
            attr = ctx.attr,
            direct_module_deps = ctx.attr.deps,
            direct_cross_library_deps = ctx.attr.cross_library_deps,
            transitive_module_dep_labels = transitive_module_dep_labels,
        ),
    ]
