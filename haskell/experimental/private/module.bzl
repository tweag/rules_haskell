load("@bazel_skylib//lib:paths.bzl", "paths")
load(
    "//haskell:private/dependencies.bzl",
    "gather_dep_info",
)
load(
    "//haskell:private/expansions.bzl",
    "expand_make_variables",
    "haskell_library_expand_make_variables",
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
load("//haskell:private/set.bzl", "set")
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
load(
    "//haskell:private/actions/process_hsc_file.bzl",
    "preprocess_hsc_flags_and_inputs",
    "process_hsc_file",
)

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
        abi_inputs,
        object_inputs,
        narrowed_objects,
        extra_ldflags_file,
        module):
    """Build a module. Returns data needed by haskell_repl.

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

    Returns:
      struct(source_file, boot_file, import_dir, user_compile_flags):
        source_file: The file that contains the Haskell module. None if it's a boot file.
        boot_file: The file that contains the boot Haskell module. None if it's not a boot file.
        import_dir: A possibly (e.g. due to hsc) newly generated import directory.
        user_compile_flags: Compiler flags specified by the user in this module after location expansion.
    """

    version = getattr(ctx.attr, "version", None)
    moduleAttr = module[HaskellModuleInfo].attr

    # Collect dependencies
    src = moduleAttr.src.files.to_list()[0]
    extra_srcs = [f for t in moduleAttr.extra_srcs + ctx.attr.extra_srcs for f in t.files.to_list()]

    user_ghcopts = []
    user_ghcopts += haskell_library_expand_make_variables("ghcopts", ctx, ctx.attr.ghcopts)

    module_extra_attrs = [
        [moduleAttr.src],
        moduleAttr.extra_srcs,
        moduleAttr.plugins,
        moduleAttr.tools,
    ]

    user_compile_flags = expand_make_variables("ghcopts", ctx, moduleAttr.ghcopts, module_extra_attrs)
    user_ghcopts += user_compile_flags

    import_dir = None
    if src.extension == "hsc":
        hsc_flags, hsc_inputs = preprocess_hsc_flags_and_inputs(dep_info, user_ghcopts, version)

        hs_out, idir = process_hsc_file(hs, cc, hsc_flags, hsc_inputs, src)
        src = hs_out
        import_dir = idir

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
            version = version,
        ),
        plugin_pkg_info = expose_packages(
            package_ids = [
                pkg_id
                for plugin in plugins
                for pkg_id in all_dependencies_package_ids(plugin.deps)
            ],
            package_databases = plugin_dep_info.package_databases,
            version = version,
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
    args.add_all(user_ghcopts)

    if plugins and not enable_th:
        # For #1681. These suppresses bogus warnings about missing libraries which
        # aren't really needed.
        args.add("-Wno-missed-extra-shared-lib")
    if enable_th:
        args.add_all(narrowed_objects)

    outputs = [module_outputs.hi, module_outputs.abi]
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

    module_name = module.label.package.replace("/", "_") + "_" + module.label.name

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
                abi_inputs,
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
        interface_inputs = interface_inputs,
        extra_name = module_name,
        hi_file = module_outputs.hi,
        abi_file = module_outputs.abi,
    )

    is_boot = _is_boot(src.path)

    return struct(
        source_file = None if is_boot else src,
        boot_file = src if is_boot else None,
        import_dir = import_dir,
        user_compile_flags = user_compile_flags,
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

def _is_boot(path):
    return paths.split_extension(path)[1] in [".hs-boot", ".lhs-boot"]

def _declare_module_outputs(hs, with_profiling, with_shared, hidir, odir, module):
    module_path = get_module_path_from_target(module)

    src = module[HaskellModuleInfo].attr.src.files.to_list()[0].path
    hs_boot = _is_boot(src)
    extension_template = "%s"
    if hs_boot:
        extension_template = extension_template + "-boot"
    if with_profiling:
        extension_template = "p_" + extension_template
    extension_template = module_path + "." + extension_template

    hi = hs.actions.declare_file(paths.join(hidir, extension_template % "hi"))
    abi = hs.actions.declare_file(paths.join(hidir, extension_template % "abi"))
    o = None if hs_boot else hs.actions.declare_file(paths.join(odir, extension_template % "o"))
    if with_shared:
        dyn_o = None if hs_boot else hs.actions.declare_file(paths.join(odir, extension_template % "dyn_o"))
        dyn_hi = hs.actions.declare_file(paths.join(hidir, extension_template % "dyn_hi"))
    else:
        dyn_hi = None
        dyn_o = None
    return struct(hi = hi, dyn_hi = dyn_hi, o = o, dyn_o = dyn_o, abi = abi)

def _collect_module_outputs_of_direct_deps(with_shared, module_outputs, dep):
    his = [
        module_outputs[m.label].hi
        for m in dep[HaskellModuleInfo].direct_module_deps
        if m.label in module_outputs
    ]
    abis = [
        module_outputs[m.label].abi
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
    return his, abis, os, dyn_os

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

def _collect_narrowed_direct_deps(ctx_label, per_module_tag, dep):
    direct_cross_library_deps = dep[HaskellModuleInfo].direct_cross_library_deps
    direct_deps = [
        per_module_tag[m.label]
        for m in direct_cross_library_deps
        if m.label in per_module_tag
    ]
    if len(direct_deps) < len(direct_cross_library_deps):
        missing = [str(m.label) for m in direct_cross_library_deps if not m.label in per_module_tag]
        fail("The following dependencies of {} can't be found in 'narrowed_deps' of {}: {}".format(
            dep.label,
            ctx_label,
            ", ".join(missing),
        ))

    return direct_deps

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
    abis = {}
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
        abis.update(lib_info.per_module_abi)
        _merge_depset_dicts(transitive_interfaces, lib_info.per_module_transitive_interfaces)
        _merge_depset_dicts(transitive_objects, lib_info.per_module_transitive_objects)
        _merge_depset_dicts(transitive_dyn_objects, lib_info.per_module_transitive_dyn_objects)
    return struct(
        abis = abis,
        transitive_interfaces = transitive_interfaces,
        transitive_objects = transitive_objects,
        transitive_dyn_objects = transitive_dyn_objects,
    )

def interfaces_as_list(with_shared, o):
    if with_shared:
        return [o.hi, o.dyn_hi, o.abi]
    else:
        return [o.hi, o.abi]

# Note [On the ABI hash]
#
# The ABI hash is contained in the interface file and is the piece of
# information used by GHC to know if recompilation is necessary. The
# details on GHC recompilation avoidance can be found at:
# https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/recompilation-avoidance
#
# Whenever one compiles a module, it produces an interface file which
# contains a lot of information. It is modified often and it is
# required to compile the files which depend on this module. The
# interface file contains an ABI hash computed from the minimal
# information required to know if recompilation is required and is
# modified less often.
#
# We aim to recompile only when the ABI hash changes. So we set the
# inputs of a module to signal this to bazel. The run_ghc action takes
# as input not only the transitive closure of the interface files
# required, but also the ABI files of the direct dependencies of the
# module. Then, we use the unused_inputs_list to pretend that the
# interface files are "unused", hence the caching mechanism of Bazel
# will not rebuild on changes to the interface files that do not
# affect the ABI hashes, thus saving us some useless compilation.
#
# On the other hand, when the ABI hash changes, bazel will rebuild
# modules because they have been fed as inputs.
#
# WARNING: We realize this deviates from the intended use of
# unused_inputs_list, but there doesn't seem to be another way to
# signal to bazel which changes on an input are irrelevant and which
# changes require recompilation.
#
# There are 3 cases of imports that build_haskell_modules handles:
#  - When the module imports modules from the current library,
#    their interface files are in interface_inputs, which is the
#    unused_inputs_list passed to run_ghc, and the abi files are
#    in abi_inputs, hence passed as inputs too.
#  - When the module imports modules from other narrowed libraries,
#    their interface files are in interface_inputs as well.
#    In this case we should pass as inputs to the ghc_wrapper the
#    abi files of the modules that it imports directly from those
#    libraries. Those are the narrowed_abis which are also in
#    abi_inputs.
#  - When the module imports modules from other non-narrowed libraries
#    (e.g. base, haskell_cabal_librarys, or haskell_librarys that
#    don't use the modules attribute), there is no information
#    available about which of these libraries provide which of the
#    modules that aren't from the enclosing library or from narrowed
#    libraries.
#    In this scenario, we can't produce abi files for these modules,
#    but on the other hand their interface files are stored in dep_info_i,
#    which is not passed to unused_inputs_list. The net effect is that
#    modules are recompiled if any non-narrowed libraries are changed.

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
        and in their dependencies. The repl_info returned here aggregates data
        from all the haskell_modules being run here.

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
      struct(his, dyn_his, os, dyn_os, per_module_transitive_interfaces, per_module_transitive_objects, per_module_transitive_dyn_objects, repl_info):
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
        repl_info: struct(source_files, boot_files, import_dirs, user_compile_flags):
          source_files: Depset of files that contain Haskell modules.
          boot_files: Depset of files that contain Haskell boot modules.
          import_dirs: Set of newly generated import directories. hsc2hs generates these.
          user_compile_flags: Compiler flags specified by the user, after location expansion.
    """
    per_module_maps = _merge_narrowed_deps_dicts(ctx.label, ctx.attr.narrowed_deps)

    # We produce separate infos for narrowed_deps and deps because the module
    # files in dep_info are given as inputs to the build action, but the
    # modules files from narrowed_deps_info are only given when haskell_module
    # declares to depend on them.

    # dep_info contains the dependency to non-narrowed external libraries.
    dep_info = gather_dep_info(ctx.attr.name, ctx.attr.deps)
    narrowed_deps_info = gather_dep_info(ctx.attr.name, ctx.attr.narrowed_deps)
    all_deps_info = gather_dep_info(ctx.attr.name, ctx.attr.deps + ctx.attr.narrowed_deps)
    empty_deps_info = gather_dep_info(ctx.attr.name, [])
    transitive_module_deps = _reorder_module_deps_to_postorder(ctx.label, ctx.attr.modules)
    module_outputs = {dep.label: _declare_module_outputs(hs, with_profiling, with_shared, hidir, odir, dep) for dep in transitive_module_deps}

    module_interfaces = {}
    module_objects = {}
    module_dyn_objects = {}

    source_files = []
    boot_files = []
    import_dirs = []
    user_compile_flags = []

    for dep in transitive_module_deps:
        # called in all cases to validate cross_library_deps, although the output
        # might be ignored when disabling narrowing
        narrowed_interfaces = _collect_narrowed_deps_module_files(ctx.label, per_module_maps.transitive_interfaces, dep)

        # See Note [On the ABI hash]
        narrowed_abis = _collect_narrowed_direct_deps(ctx.label, per_module_maps.abis, dep)
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

        his, abis, os, dyn_os = _collect_module_outputs_of_direct_deps(with_shared, module_outputs, dep)

        # interface_inputs collects the interfaces of the transitive dependencies to a module from the library we are compiling,
        # and the narrowed dependencies.
        interface_inputs = _collect_module_inputs(module_interfaces, narrowed_interfaces, his, dep)

        # Similarly abi_inputs contains the direct dependencies to a module from the library we are compiling,
        # and the narrowed dependencies.
        # One only wants the direct abi files, since if a modifiaction in a transitive dependency did not affect any abi of a direct dependency,
        # it means that those changes do not impact the file we are considering, hence recompilation can be avoided.
        abi_inputs = depset(direct = abis + narrowed_abis)

        object_inputs = depset(transitive = [
            _collect_module_inputs(module_objects, narrowed_objects, os, dep),
            _collect_module_inputs(module_dyn_objects, narrowed_objects, dyn_os, dep),
        ])

        module_output = _build_haskell_module(
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
            abi_inputs,
            object_inputs,
            narrowed_objects,
            extra_ldflags_file,
            dep,
        )
        if module_output.source_file:
            source_files.append(module_output.source_file)
        if module_output.boot_file:
            boot_files.append(module_output.boot_file)
        if module_output.import_dir:
            import_dirs.append(module_output.import_dir)
        user_compile_flags += module_output.user_compile_flags

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
    per_module_abi0 = {
        dep.label: module_outputs[dep.label].abi
        for dep in transitive_module_deps
    }
    _merge_depset_dicts(per_module_transitive_interfaces0, per_module_maps.transitive_interfaces)
    _merge_depset_dicts(per_module_transitive_objects0, per_module_maps.transitive_objects)
    _merge_depset_dicts(per_module_transitive_dyn_objects0, per_module_maps.transitive_dyn_objects)
    _merge_depset_dicts(per_module_abi0, per_module_maps.abis)

    return struct(
        his = hi_set,
        dyn_his = dyn_hi_set,
        os = o_set,
        dyn_os = dyn_o_set,
        per_module_transitive_interfaces = per_module_transitive_interfaces0,
        per_module_transitive_objects = per_module_transitive_objects0,
        per_module_transitive_dyn_objects = per_module_transitive_dyn_objects0,
        per_module_abi = per_module_abi0,
        repl_info = struct(
            source_files = depset(source_files),
            boot_files = depset(boot_files),
            import_dirs = set.from_list(import_dirs),
            user_compile_flags = user_compile_flags,
        ),
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

    result = [
        DefaultInfo(),
        HaskellModuleInfo(
            attr = ctx.attr,
            direct_module_deps = ctx.attr.deps,
            direct_cross_library_deps = ctx.attr.cross_library_deps,
            transitive_module_dep_labels = transitive_module_dep_labels,
        ),
    ]

    return result
