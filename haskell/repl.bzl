"""Multi target Haskell REPL."""

load("@bazel_skylib//lib:new_sets.bzl", "sets")
load("@bazel_skylib//lib:paths.bzl", "paths")
load("@bazel_skylib//lib:shell.bzl", "shell")
load("@rules_cc//cc:find_cc_toolchain.bzl", "find_cc_toolchain", "use_cc_toolchain")
load(":cc.bzl", "ghc_cc_program_args")
load(
    ":private/cc_libraries.bzl",
    "deps_HaskellCcLibrariesInfo",
    "get_cc_libraries",
    "get_ghci_library_files",
    "get_library_files",
    "haskell_cc_libraries_aspect",
    "link_libraries",
    "merge_HaskellCcLibrariesInfo",
    "merge_cc_shared_library_infos",
)
load(":private/context.bzl", "haskell_context", "render_env")
load(":private/expansions.bzl", "expand_make_variables")
load(":private/java.bzl", "java_interop_info")
load(
    ":private/path_utils.bzl",
    "match_label",
    "parse_pattern",
    "target_unique_name",
)
load(":private/set.bzl", "set")
load(
    ":providers.bzl",
    "HaskellCcLibrariesInfo",
    "HaskellInfo",
    "HaskellLibraryInfo",
    "HaskellToolchainLibraryInfo",
    "all_package_ids",
)

HaskellReplLoadInfo = provider(
    doc = """Haskell REPL target information.

    Information for a Haskell target to load into the REPL as source.
    """,
    fields = {
        "package_id": "package_id for a specific dependency. \"\" after collection.",
        "dep_package_ids": "package_ids of direct dependencies.",
        "source_files": "Depset of files that contain Haskell modules.",
        "boot_files": "Depset of Haskell boot files.",
        "module_names": "Depset of Haskell module names to load.",
        "import_dirs": "Depset of Haskell import directories.",
        "cc_libraries_info": "HaskellCcLibrariesInfo of transitive C dependencies.",
        "cc_info": "CcInfo of transitive C dependencies.",
        "cc_shared_library_infos": "CcSharedLibraryInfo providers of transitive C dependencies.",
        "compiler_flags": "Flags to pass to the Haskell compiler.",
        "repl_ghci_args": "Arbitrary extra arguments to pass to GHCi. This extends `compiler_flags` and `repl_ghci_args` from the toolchain",
        "data_runfiles": "Runtime data dependencies of this target, i.e. the files and runfiles of the `data` attribute.",
        "java_deps": "depset of Files to jars needed for building.",
    },
)

HaskellReplDepInfo = provider(
    doc = """Haskell REPL dependency information.

    Information for a Haskell target to load into the REPL as a built package or
    a sibling unit.
    """,
    fields = {
        "direct_package_ids": "List of package ids for this specific dependency",
        "package_ids": "Depset of workspace unique package identifiers.",
        "package_databases": "Set of package cache files.",
        "interface_dirs": "Set of interface dirs for all the dependencies",
        "cc_libraries_info": "HaskellCcLibrariesInfo of transitive C dependencies.",
        "cc_info": "CcInfo of the package itself (includes its transitive dependencies).",
        "cc_shared_library_infos": "CcSharedLibraryInfo providers of transitive C dependencies.",
        "runfiles": "Runfiles of this target.",
    },
)

HaskellReplCollectInfo = provider(
    doc = """Collect Haskell REPL information.

    Holds information to generate a REPL that loads some targets as source
    and some targets as built packages.
    """,
    fields = {
        "load_infos": "Dictionary from labels to HaskellReplLoadInfo.",
        "dep_infos": "Dictionary from labels to HaskellReplDepInfo.",
        # The direct dependencies are a depset for immutability, so they can be placed in a depset.
        "haskell_targets_postorder": "Depset of haskell targets in postorder. Tuple containing (label, direct haskell dependencies depset).",
    },
)

HaskellReplInfo = provider(
    doc = """Haskell REPL information.

    Holds information to generate a REPL that loads a specific set of targets
    from source or as built packages.
    """,
    fields = {
        "load_info": "Combined HaskellReplLoadInfo.",
        "dep_info": "Combined HaskellReplDepInfo.",
    },
)

HaskellMultiReplInfo = provider(
    doc = """Haskell Multi-REPL information.

    Starting with GHC 9.4, GHCi can be passed multiple units with different
    compiler flags for each unit by placing each unit in a separate file and
    passing -unit @unitfile for each unit.

    Holds information needed to generate unit files, one for each item in
    load_infos.
    """,
    fields = {
        "repl_infos": "Dictionary from labels to HaskellReplInfo.",
        "label_order": "Depset of labels for which to produce unit files.",
    },
)

def _merge_runfiles(runfiles_list):
    result = None
    for runfiles in runfiles_list:
        if result == None:
            result = runfiles
        elif runfiles != None:
            result = result.merge(runfiles)
    return result

def _data_runfiles(ctx, rule, attr):
    """Generate runfiles for a data attribute.

    Attrs:
      ctx: The rule context.
      rule: The rule object, `ctx` for a rule, `ctx.rule` for an aspect.
      attr: The attribute name of the data attribute.

    Returns:
      A runfiles object capturing data files and data runfiles.
    """
    return _merge_runfiles(
        [ctx.runfiles(files = getattr(rule.files, attr, []))] +
        [data[DefaultInfo].default_runfiles for data in getattr(rule.attr, attr, [])],
    )

def _merge_HaskellReplLoadInfo(load_infos):
    source_files = depset()
    boot_files = depset()
    module_names = depset()
    import_dirs = depset()
    cc_libraries_infos = []
    cc_infos = []
    cc_shared_library_infos = []
    compiler_flags = []
    repl_ghci_args = []
    data_runfiles = []
    java_deps = []

    for load_info in load_infos:
        source_files = depset(transitive = [source_files, load_info.source_files])
        boot_files = depset(transitive = [boot_files, load_info.boot_files])
        module_names = depset(transitive = [module_names, load_info.module_names])
        import_dirs = depset(transitive = [import_dirs, load_info.import_dirs])
        cc_libraries_infos.append(load_info.cc_libraries_info)
        cc_infos.append(load_info.cc_info)
        cc_shared_library_infos.extend(load_info.cc_shared_library_infos)
        compiler_flags += load_info.compiler_flags
        repl_ghci_args += load_info.repl_ghci_args
        data_runfiles.append(load_info.data_runfiles)
        java_deps.append(load_info.java_deps)

    return HaskellReplLoadInfo(
        package_id = "",
        source_files = source_files,
        boot_files = boot_files,
        module_names = module_names,
        import_dirs = import_dirs,
        cc_libraries_info = merge_HaskellCcLibrariesInfo(infos = cc_libraries_infos),
        cc_info = cc_common.merge_cc_infos(cc_infos = cc_infos),
        cc_shared_library_infos = cc_shared_library_infos,
        compiler_flags = compiler_flags,
        repl_ghci_args = repl_ghci_args,
        data_runfiles = _merge_runfiles(data_runfiles),
        java_deps = depset(transitive = java_deps),
    )

def _merge_HaskellReplLoadInfoMulti(root_info, load_infos):
    cc_libraries_infos = []
    cc_infos = []
    cc_shared_library_infos = []
    data_runfiles = []
    java_deps = []
    for load_info in load_infos:
        cc_libraries_infos.append(load_info.cc_libraries_info)
        cc_infos.append(load_info.cc_info)
        cc_shared_library_infos.extend(load_info.cc_shared_library_infos)
        data_runfiles.append(load_info.data_runfiles)
        java_deps.append(load_info.java_deps)

    return HaskellReplLoadInfo(
        package_id = root_info.package_id,
        dep_package_ids = root_info.dep_package_ids,
        source_files = root_info.source_files,
        boot_files = root_info.boot_files,
        module_names = root_info.module_names,
        import_dirs = root_info.import_dirs,
        cc_libraries_info = merge_HaskellCcLibrariesInfo(infos = cc_libraries_infos),
        cc_info = cc_common.merge_cc_infos(cc_infos = cc_infos),
        cc_shared_library_infos = cc_shared_library_infos,
        compiler_flags = root_info.compiler_flags,
        repl_ghci_args = root_info.repl_ghci_args,
        data_runfiles = _merge_runfiles(data_runfiles),
        java_deps = depset(transitive = java_deps),
    )

def _merge_HaskellReplDepInfo(dep_infos, dep_infos_for_package_dbs = []):
    package_ids = depset()
    package_databases = depset()
    interface_dirs = depset()
    cc_libraries_infos = []
    cc_infos = []
    cc_shared_library_infos = []
    runfiles = []

    for dep_info in dep_infos:
        package_ids = depset(transitive = [package_ids, dep_info.package_ids])
        package_databases = depset(transitive = [package_databases, dep_info.package_databases])
        interface_dirs = depset(transitive = [interface_dirs, dep_info.interface_dirs])
        cc_libraries_infos.append(dep_info.cc_libraries_info)
        cc_infos.append(dep_info.cc_info)
        cc_shared_library_infos.extend(dep_info.cc_shared_library_infos)
        runfiles.append(dep_info.runfiles)

    for dep_info in dep_infos_for_package_dbs:
        package_databases = depset(transitive = [package_databases, dep_info.package_databases])

    return HaskellReplDepInfo(
        direct_package_ids = [],
        package_ids = package_ids,
        package_databases = package_databases,
        interface_dirs = interface_dirs,
        cc_libraries_info = merge_HaskellCcLibrariesInfo(infos = cc_libraries_infos),
        cc_info = cc_common.merge_cc_infos(cc_infos = cc_infos),
        cc_shared_library_infos = cc_shared_library_infos,
        runfiles = _merge_runfiles(runfiles),
    )

def _create_HaskellReplCollectInfo(target, dep_labels, dep_package_ids, dep_package_ids_transitive, ctx):
    load_infos = {}
    dep_infos = {}

    hs_info = target[HaskellInfo]
    package_id = ""

    load_info = None
    dep_info = None

    if not HaskellToolchainLibraryInfo in target:
        java_deps_list = []

        if hasattr(ctx.rule.attr, "deps"):
            java_deps_list.append(java_interop_info(ctx.rule.attr.deps).inputs)

        # TODO[GL]: add tests for the java deps in narrowed_deps
        if hasattr(ctx.rule.attr, "narrowed_deps"):
            java_deps_list.append(java_interop_info(ctx.rule.attr.narrowed_deps).inputs)

        java_deps = depset(transitive = java_deps_list)

        # TODO[GL]: add tests for CcInfo deps in narrowed_deps
        cc_info_deps = [
            dep
            for dep in getattr(ctx.rule.attr, "deps", []) + getattr(ctx.rule.attr, "narrowed_deps", [])
            if CcInfo in dep and not HaskellInfo in dep
        ]
        cc_shared_library_info_deps = [
            dep
            for dep in getattr(ctx.rule.attr, "deps", []) + getattr(ctx.rule.attr, "narrowed_deps", [])
            if CcSharedLibraryInfo in dep and not HaskellInfo in dep
        ]
        if HaskellLibraryInfo in target:
            package_id = target[HaskellLibraryInfo].package_id
        load_info = HaskellReplLoadInfo(
            package_id = package_id,
            dep_package_ids = dep_package_ids,
            source_files = hs_info.source_files,
            boot_files = hs_info.boot_files,
            module_names = hs_info.module_names,
            import_dirs = set.to_depset(hs_info.import_dirs),
            cc_libraries_info = deps_HaskellCcLibrariesInfo(cc_info_deps + cc_shared_library_info_deps),
            cc_info = cc_common.merge_cc_infos(cc_infos = [
                # Collect pure C library dependencies, no Haskell dependencies.
                dep[CcInfo]
                for dep in cc_info_deps
            ]),
            cc_shared_library_infos = [merge_cc_shared_library_infos(
                owner = ctx.label,
                cc_shared_library_infos = [
                    dep[CcSharedLibraryInfo]
                    for dep in cc_shared_library_info_deps
                ],
            )],
            compiler_flags = hs_info.user_compile_flags,
            repl_ghci_args = hs_info.user_repl_flags,
            data_runfiles = _data_runfiles(ctx, ctx.rule, "data"),
            java_deps = java_deps,
        )
        load_infos[target.label] = load_info
    if HaskellLibraryInfo in target:
        lib_info = target[HaskellLibraryInfo]
        dep_info = HaskellReplDepInfo(
            direct_package_ids = all_package_ids(lib_info),
            package_ids = depset(order = "postorder", direct = all_package_ids(lib_info), transitive = dep_package_ids_transitive),
            package_databases = hs_info.package_databases,
            interface_dirs = hs_info.interface_dirs,
            cc_libraries_info = target[HaskellCcLibrariesInfo],
            cc_info = target[CcInfo],
            cc_shared_library_infos = [target[CcSharedLibraryInfo]] if CcSharedLibraryInfo in target else [],
            runfiles = target[DefaultInfo].default_runfiles,
        )
        dep_infos[target.label] = dep_info

    return HaskellReplCollectInfo(
        load_infos = load_infos,
        dep_infos = dep_infos,
        haskell_targets_postorder =
            depset(order = "postorder", direct = [(target.label, dep_labels)]),
    )

def _merge_HaskellReplCollectInfo(root_args, dep_args):
    load_infos = {}
    dep_infos = {}
    haskell_targets_root = depset()
    for arg in root_args:
        load_infos.update(arg.load_infos)
        dep_infos.update(arg.dep_infos)
        haskell_targets_root = depset(transitive = [haskell_targets_root, arg.haskell_targets_postorder])

    transitive_targets = []
    for arg in dep_args:
        load_infos.update(arg.load_infos)
        dep_infos.update(arg.dep_infos)
        transitive_targets.append(arg.haskell_targets_postorder)

    return HaskellReplCollectInfo(
        load_infos = load_infos,
        dep_infos = dep_infos,
        haskell_targets_postorder = depset(
            order = "postorder",
            direct = haskell_targets_root.to_list(),
            transitive = transitive_targets,
        ),
    )

def _load_as_source(from_source, from_binary, lbl):
    """Whether a package should be loaded by source or as binary."""
    for pat in from_binary:
        if match_label(pat, lbl):
            return False

    for pat in from_source:
        if match_label(pat, lbl):
            return True

    return False

def _create_HaskellReplInfo(from_source, from_binary, collect_info):
    """Convert a HaskellReplCollectInfo to a HaskellReplInfo.

    Args:
      from_source: List of patterns for packages to load by source.
      from_binary: List of patterns for packages to load as binary packages.
      collect_info: HaskellReplCollectInfo provider.

    Returns:
      HaskellReplInfo provider.
    """
    load_infos = collect_info.load_infos
    dep_infos = collect_info.dep_infos

    # Collect all packages to load by source.
    load_info = _merge_HaskellReplLoadInfo([
        load_info
        for (lbl, load_info) in load_infos.items()
        if _load_as_source(from_source, from_binary, lbl)
    ])

    # Collect all packages to load as binary packages.
    dep_info = _merge_HaskellReplDepInfo([
        dep_info
        for (lbl, dep_info) in dep_infos.items()
        if not (lbl in load_infos and _load_as_source(from_source, from_binary, lbl))
    ])

    return HaskellReplInfo(
        load_info = load_info,
        dep_info = dep_info,
    )

def _create_HaskellMultiReplInfo(from_source, from_binary, collect_info):
    """Convert a HaskellReplCollectInfo to a HaskellMultiReplInfo.

    Args:
      from_source: List of patterns for packages to load by source.
      from_binary: List of patterns for packages to load as binary packages.
      collect_info: HaskellReplCollectInfo provider.

    Returns:
      HaskellMultiReplInfo provider.
    """

    load_infos = {}
    dep_infos = {}
    repl_infos = {}
    label_order = depset(order = "preorder")
    local_labels = sets.make()

    targets_ordered = collect_info.haskell_targets_postorder.to_list()
    for (label, direct_deps) in targets_ordered:
        load_info = collect_info.load_infos.get(label)
        dep_info = collect_info.dep_infos.get(label)
        merged_load_info = None
        merged_dep_info = None
        deps_list = direct_deps.to_list()
        load_as_source = _load_as_source(from_source, from_binary, label)
        if load_info:
            merged_load_info = _merge_HaskellReplLoadInfoMulti(
                load_info,
                [
                    load_infos[label]
                    for label in deps_list
                    if label in load_infos
                ],
            )
            load_infos[label] = merged_load_info
        dep_infos_to_merge = [
            dep_infos[label]
            for label in deps_list
            if label in dep_infos and not sets.contains(local_labels, label)
        ]
        dep_infos_for_package_dbs = [
            dep_infos[label]
            for label in deps_list
            if label in dep_infos and sets.contains(local_labels, label)
        ]
        merged_dep_info = _merge_HaskellReplDepInfo(dep_infos_to_merge, dep_infos_for_package_dbs)
        if dep_info and not load_as_source:
            dep_info_with_self = _merge_HaskellReplDepInfo([dep_info, merged_dep_info])
        else:
            dep_info_with_self = merged_dep_info
        dep_infos[label] = dep_info_with_self

        if not load_as_source:
            continue
        if not merged_load_info or not merged_dep_info:
            continue
        repl_info = HaskellReplInfo(
            load_info = merged_load_info,
            dep_info = merged_dep_info,
        )
        label_order = depset(direct = [label], transitive = [label_order])
        repl_infos[label] = repl_info

        # Don't merge the dep_info for any locally loaded unit when merging later
        sets.insert(local_labels, label)

    return HaskellMultiReplInfo(
        repl_infos = repl_infos,
        label_order = label_order,
    )

def _concat(lists):
    return [item for sublist in lists for item in sublist]

def _compiler_flags_and_inputs(hs, cc, repl_info, get_dirname, static = False, include_package_ids = True):
    """Collect compiler flags and inputs.

    Compiler flags:
      - Package databases and identifiers.
      - Linker flags for C library dependencies.
      - Haskell include directory flags.

    Inputs:
      - Source files.
      - Package databases.
      - C library dependencies.
      - Locale archive if required.
      - Required CC toolchain inputs.

    Args:
      hs: Haskell context.
      cc: CcToolchainInfo.
      repl_info: HaskellReplInfo.
      static: bool, Whether we're collecting libraries for static RTS.
        Contrary to GHCi, ghcide is built as a static executable using the static RTS.
      get_dirname: File -> string, customize the get_dirname function used for package db paths.

    Returns:
      (args, inputs):
        args: list of string, the compiler flags.
        inputs: depset of File, inputs required by the compiler.
    """
    args = []

    # Load built dependencies (-package-id, -package-db)
    if include_package_ids:
        for package_id in repl_info.dep_info.package_ids.to_list():
            args.extend(["-package-id", package_id])
    for package_cache in repl_info.dep_info.package_databases.to_list():
        args.extend(["-package-db", get_dirname(package_cache)])

    # Load C library dependencies
    cc_libraries_info = merge_HaskellCcLibrariesInfo(infos = [
        repl_info.load_info.cc_libraries_info,
        repl_info.dep_info.cc_libraries_info,
    ])
    cc_info = cc_common.merge_cc_infos(cc_infos = [
        repl_info.load_info.cc_info,
        repl_info.dep_info.cc_info,
    ])
    cc_shared_library_infos = repl_info.load_info.cc_shared_library_infos + repl_info.dep_info.cc_shared_library_infos
    linker_inputs = cc_info.linking_context.linker_inputs.to_list() + [info.linker_input for info in cc_shared_library_infos]
    all_libraries = [lib for li in linker_inputs for lib in li.libraries]
    cc_libraries = get_cc_libraries(cc_libraries_info, all_libraries)
    if static:
        cc_library_files = _concat(get_library_files(hs, cc_libraries_info, cc_libraries))
    else:
        cc_library_files = get_ghci_library_files(hs, cc_libraries_info, cc_libraries)

    link_libraries(cc_library_files, args, get_dirname)

    if static:
        all_library_files = _concat(get_library_files(hs, cc_libraries_info, all_libraries, include_real_paths = True))
    else:
        all_library_files = get_ghci_library_files(hs, cc_libraries_info, all_libraries, include_real_paths = True)

    inputs = depset(transitive = [
        repl_info.load_info.source_files,
        repl_info.dep_info.package_databases,
        depset(all_library_files),
        depset([hs.toolchain.locale_archive] if hs.toolchain.locale_archive else []),
        cc.all_files,
        hs.toolchain.cc_wrapper.runfiles.files,
        repl_info.dep_info.interface_dirs,
    ])
    return (args, inputs)

def _haskell_repl_impl(ctx):
    """Build a multi target REPL.

    Args:
      ctx: Rule context.

    Returns:
      List of providers:
        DefaultInfo provider for the executable REPL script.

    """
    repl_info = _repl_info(ctx)
    hs = haskell_context(ctx)
    cc = find_cc_toolchain(ctx)
    output = ctx.outputs.repl

    # The base and directory packages are necessary for the GHCi script we use
    # (loads source files and brings in scope the corresponding modules).
    args = ["-hide-all-packages", "-package", "base", "-package", "directory"]

    # REPL scripts `cd` into the workspace root. Depending on their provenance,
    # some C libraries' files may be in subdirectories which are _only_ relative
    # to the execroot. External static C library dependencies are an example of
    # this -- unchanged we may end up with paths like
    # `external/some_dependency/lib` and/or
    # `bazel-out/k8-fastbuild/bin/_solib_k8/...`; the former containing the
    # archive (`.a`) file we want, but only being relative to the execroot, and
    # the latter being relative to both the workspace root and the execroot but
    # only containing dynamic libraries.
    #
    # We fix this by prefixing paths with the execroot when generating linker
    # flags so that all required libraries are visible.
    compiler_flags, inputs = _compiler_flags_and_inputs(
        hs,
        cc,
        repl_info,
        get_dirname = lambda f: paths.join("$RULES_HASKELL_EXEC_ROOT", f.dirname),
    )
    args.extend(compiler_flags)
    cc_path = paths.join(
        "$RULES_HASKELL_EXEC_ROOT",
        hs.toolchain.cc_wrapper.executable.path,
    )
    ld_path = paths.join(
        "$RULES_HASKELL_EXEC_ROOT",
        cc.ld_executable,
    )
    args.extend(['"{}"'.format(arg) for arg in ghc_cc_program_args(hs, cc_path, ld_path)])

    # Load source files
    # Force loading by source with `:add *...`.
    # See https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html#ghci-cmd-:add
    add_sources = [
        "*" + f.path
        for f in repl_info.load_info.source_files.to_list()
    ]
    ghci_repl_script = hs.actions.declare_file(
        target_unique_name(hs, "ghci-repl-script"),
    )
    hs.actions.expand_template(
        template = ctx.file._ghci_repl_script,
        output = ghci_repl_script,
        substitutions = {
            "{ADD_SOURCES}": " ".join(add_sources),
            "{MODULES}": " ".join(repl_info.load_info.module_names.to_list()),
            "{COMMANDS}": "\n".join(ctx.attr.repl_ghci_commands),
        },
    )
    args += [
        "-ghci-script",
        paths.join("$RULES_HASKELL_EXEC_ROOT", ghci_repl_script.path),
    ]

    # Extra arguments.
    # `compiler flags` is the default set of arguments for the repl,
    # augmented by `repl_ghci_args`.
    # The ordering is important, first compiler flags (from toolchain
    # and local rule), then from `repl_ghci_args`. This way the more
    # specific arguments are listed last, and then have more priority in
    # GHC.
    # Note that most flags for GHCI do have their negative value, so a
    # negative flag in `repl_ghci_args` can disable a positive flag set
    # in `compiler_flags`, such as `-XNoOverloadedStrings` will disable
    # `-XOverloadedStrings`.
    repl_ghci_args = expand_make_variables(
        "repl_ghci_args",
        ctx,
        ctx.attr.repl_ghci_args,
        [ctx.attr.data],
    )
    quote_args = (
        hs.toolchain.ghcopts +
        repl_info.load_info.compiler_flags +
        hs.toolchain.repl_ghci_args +
        repl_info.load_info.repl_ghci_args +
        repl_ghci_args
    )

    env = dict(hs.env)

    classpath_inputs = [
        paths.join("$RULES_HASKELL_EXEC_ROOT", f.path)
        for f in repl_info.load_info.java_deps.to_list()
    ]
    if len(classpath_inputs) > 0:
        env["CLASSPATH"] = ":".join(classpath_inputs)

    # These env-vars refer to the build-time GHC distribution. However, if the
    # REPL uses ghc-paths then it should refer to the runtime GHC distribution,
    # i.e. the one included in the runfiles. Therefore we remove these env-vars
    # to force ghc-paths to look them up in the runfiles.
    env.pop("RULES_HASKELL_GHC_PATH")
    env.pop("RULES_HASKELL_GHC_PKG_PATH")
    env.pop("RULES_HASKELL_LIBDIR_PATH")
    env.pop("RULES_HASKELL_DOCDIR_PATH")

    hs.actions.expand_template(
        template = ctx.file._ghci_repl_wrapper,
        output = output,
        is_executable = True,
        substitutions = {
            "%{ENV}": render_env(env),
            "%{TOOL}": hs.tools.ghc.path,
            "%{OUTPUT}": paths.dirname(output.path),
            "%{ARGS}": "(" + " ".join(
                ["--interactive"] + args + [
                    shell.quote(a)
                    for a in quote_args
                ],
            ) + ")",
        },
    )

    runfiles = [
        ctx.runfiles(
            files = [
                hs.tools.ghc,
                ghci_repl_script,
            ],
            transitive_files = inputs,
        ),
        hs.toolchain.cc_wrapper.runfiles,
    ]
    if ctx.attr.collect_data:
        runfiles.append(repl_info.load_info.data_runfiles)
        runfiles.append(repl_info.dep_info.runfiles)
        runfiles.append(_data_runfiles(ctx, ctx, "data"))

    return [DefaultInfo(
        executable = output,
        runfiles = _merge_runfiles(runfiles),
    )]

def _rlocationpath_str(ctx, shortpath):
    """ Format a shortpath in a way that is suitable for the runfiles libraries """
    return paths.normalize(paths.join(ctx.workspace_name, shortpath))

def _rlocationpath(ctx, file):
    """ Outputs a path to `file` suitable for the runfiles libraries """
    return _rlocationpath_str(ctx, file.short_path)

def _rlocation(ctx, file):
    """Outputs a bash expression computing the runtime path of a runfile using the bash runfiles library"""
    return "$(rlocation {})".format(_rlocationpath(ctx, file))

def _hie_bios_impl(ctx):
    """Build a shell script that prints the hie-bios flags for ghcide using the bash runfiles library to recover absolute paths.

    Args:
      ctx: Rule context.

    Returns:
      List of providers:
        DefaultInfo provider for the hie-bios script
"""
    repl_info = _repl_info(ctx)
    hs = haskell_context(ctx)
    cc = find_cc_toolchain(ctx)
    if ctx.attr.multi:
        return _hie_bios_impl_multi(ctx)

    args = []

    more_args, inputs = _compiler_flags_and_inputs(
        hs,
        cc,
        repl_info,
        static = True,
        get_dirname = lambda p: "$(dirname {})".format(_rlocation(ctx, p)),
    )
    args.extend(more_args)
    runfiles_depset = depset(
        direct = [hs.toolchain.cc_wrapper.executable],
        transitive = [
            repl_info.load_info.source_files,
            repl_info.load_info.boot_files,
            inputs,
        ],
    )
    runfiles = ctx.runfiles(transitive_files = runfiles_depset).merge_all(
        [
            ctx.attr._bash_runfiles[DefaultInfo].default_runfiles,
            hs.toolchain.cc_wrapper.runfiles,
        ],
    )
    cc_path = _rlocation(ctx, hs.toolchain.cc_wrapper.executable)
    ld_path = "$(rlocation {}{})".format(
        _rlocationpath_str(ctx, cc.ld_executable),
        ".exe" if hs.toolchain.is_windows else "",
    )
    args.extend(ghc_cc_program_args(hs, cc_path, ld_path))
    args.extend(hs.toolchain.ghcopts)
    args.extend(repl_info.load_info.compiler_flags)
    args.extend(["-no-user-package-db"])

    # Add import directories.
    # Note, src_strip_prefix is deprecated. However, for now ghcide depends on
    # `-i` flags to find source files to modules.
    import_dirs = [paths.join("$RULES_HASKELL_EXEC_ROOT", dir) for dir in repl_info.load_info.import_dirs.to_list()]
    for import_dir in import_dirs:
        args.append("-i" + (import_dir or "."))

    # List modules (Targets) covered by this cradle and boot files
    # We could also rely on the _rlocation function here but we do the same as for import_dirs because of the following issue:
    # https://github.com/haskell/haskell-language-server/issues/3510
    args.extend([
        paths.join("$RULES_HASKELL_EXEC_ROOT", f.path)
        for f in repl_info.load_info.source_files.to_list()
    ])
    args.extend([
        paths.join("$RULES_HASKELL_EXEC_ROOT", f.path)
        for f in repl_info.load_info.boot_files.to_list()
    ])

    hie_bios_script = hs.actions.declare_file(
        target_unique_name(hs, "hie-bios"),
    )
    hs.actions.expand_template(
        template = ctx.file._hie_bios_wrapper,
        is_executable = True,
        output = hie_bios_script,
        substitutions = {
            "%{OUTPUT}": hie_bios_script.path,
            "%{OUTPUT_RLOCATION_PATH}": _rlocationpath(ctx, hie_bios_script),
            "%{ARGS}": "\n".join(args),
            "%{UNIT_FILE_FRAGMENTS}": "",
        },
    )
    return [DefaultInfo(
        executable = hie_bios_script,
        runfiles = runfiles,
    )]

def _hie_bios_impl_multi(ctx):
    """Build a shell script that prints the hie-bios flags for ghcide.
    Specifically one that breaks out flags for different modules into different unit files.

    Args:
      ctx: Rule context.

    Returns:
      List of providers:
        DefaultInfo provider for the hie-bios script
"""
    repl_info_multi = _repl_info_multi(ctx)
    hs = haskell_context(ctx)
    cc = find_cc_toolchain(ctx)

    repl_info_labels = repl_info_multi.label_order.to_list()

    global_args = ["-hide-all-packages"]
    unit_file_fragments = []
    global_runfiles_depset = depset()

    for label in repl_info_labels:
        repl_info = repl_info_multi.repl_infos[label]
        unit_file_fragment = ctx.actions.declare_file(
            "{}_{}_{}_unit_file.sh".format(
                label.workspace_root,
                label.package,
                label.name,
            ),
        )
        unit_load_info = repl_info.load_info

        args = ["-hide-all-packages"]
        if unit_load_info.package_id:
            args.extend(["-this-unit-id", unit_load_info.package_id])

        def local_dir(f):
            if f.is_source:
                return f.dirname
            return non_local_dir(f)

        def non_local_dir(f):
            return paths.join("$RULES_HASKELL_EXEC_ROOT", f.dirname)

        def local_path(f):
            if f.is_source:
                return f.path
            return non_local_path(f)

        def non_local_path(f):
            return paths.join("$RULES_HASKELL_EXEC_ROOT", f.path)

        more_args, inputs = _compiler_flags_and_inputs(
            hs,
            cc,
            repl_info,
            static = True,
            include_package_ids = False,
            get_dirname = local_dir,
        )
        args.extend(more_args)
        for package_id in unit_load_info.dep_package_ids:
            args.extend(["-package-id", package_id])
        runfiles_depset = depset(
            direct = [hs.toolchain.cc_wrapper.executable],
            transitive = [
                repl_info.load_info.source_files,
                repl_info.load_info.boot_files,
                inputs,
            ],
        )
        global_runfiles_depset = depset(direct = [unit_file_fragment], transitive = [global_runfiles_depset, runfiles_depset])
        cc_path = _rlocation(ctx, hs.toolchain.cc_wrapper.executable)
        ld_path = "$(rlocation {}{})".format(
            _rlocationpath_str(ctx, cc.ld_executable),
            ".exe" if hs.toolchain.is_windows else "",
        )
        args.extend(["\\\"{}\\\"".format(arg) for arg in ghc_cc_program_args(hs, cc_path, ld_path)])
        args.extend(hs.toolchain.ghcopts)
        args.extend(repl_info.load_info.compiler_flags)
        args.extend(["-no-user-package-db"])

        # Add import directories.
        # Because we are trying to work with an editor, the local import
        # directories need to be non-prefixed.
        def fix_non_source(import_dir):
            if import_dir.startswith(ctx.bin_dir.path):
                return paths.join("$RULES_HASKELL_EXEC_ROOT", import_dir)
            elif import_dir.startswith(ctx.genfiles_dir.path):
                return paths.join("$RULES_HASKELL_EXEC_ROOT", import_dir)
            else:
                return import_dir

        import_dirs = [fix_non_source(dir) for dir in repl_info.load_info.import_dirs.to_list()]
        for import_dir in import_dirs:
            args.append("-i" + (import_dir or "."))

        args.extend([local_path(f) for f in repl_info.load_info.source_files.to_list()])
        args.extend([local_path(f) for f in repl_info.load_info.boot_files.to_list()])
        hs.actions.write(unit_file_fragment, "echo " + "\necho ".join(args) + "\n")
        unit_file_fragments.append(non_local_path(unit_file_fragment))

    hie_bios_script = hs.actions.declare_file(
        target_unique_name(hs, "hie-bios"),
    )
    hs.actions.expand_template(
        template = ctx.file._hie_bios_wrapper,
        is_executable = True,
        output = hie_bios_script,
        substitutions = {
            "%{OUTPUT}": hie_bios_script.path,
            "%{OUTPUT_RLOCATION_PATH}": _rlocationpath(ctx, hie_bios_script),
            "%{ARGS}": "\n".join(global_args),
            "%{UNIT_FILE_FRAGMENTS}": "\n".join(unit_file_fragments),
        },
    )
    runfiles = ctx.runfiles(transitive_files = global_runfiles_depset).merge_all(
        [
            ctx.attr._bash_runfiles[DefaultInfo].default_runfiles,
            hs.toolchain.cc_wrapper.runfiles,
        ],
    )
    return [DefaultInfo(
        executable = hie_bios_script,
        runfiles = runfiles,
    )]

def _haskell_repl_aspect_impl(target, ctx):
    # TODO[GL]: try removing this and using required_providers, once we're on a newer version of bazel
    if HaskellInfo not in target:
        return []

    deps_infos = [
        dep[HaskellReplCollectInfo]
        for deps_field_name in ["deps", "narrowed_deps"]
        for dep in getattr(ctx.rule.attr, deps_field_name, [])
        if HaskellReplCollectInfo in dep
    ]
    dep_labels = depset(direct = [
        dep.label
        for deps_field_name in ["deps", "narrowed_deps"]
        for dep in getattr(ctx.rule.attr, deps_field_name, [])
        if HaskellReplCollectInfo in dep
    ])
    dep_package_ids = []
    dep_package_ids_transitive = []

    for deps_field_name in ["deps", "narrowed_deps"]:
        for dep in getattr(ctx.rule.attr, deps_field_name, []):
            if HaskellReplCollectInfo in dep:
                rci = dep[HaskellReplCollectInfo]
                dep_info = rci.dep_infos.get(dep.label)
                if dep_info:
                    dep_package_ids_transitive.append(dep_info.package_ids)
                    dep_package_ids.extend(dep_info.direct_package_ids)

    target_info = _create_HaskellReplCollectInfo(target, dep_labels, dep_package_ids, dep_package_ids_transitive, ctx)

    collect_info = _merge_HaskellReplCollectInfo(
        [target_info],
        deps_infos,
    )

    # This aspect currently does not generate an executable REPL script by
    # itself. This could be extended in future. Note, to that end it's
    # necessary to construct a Haskell context without `ctx.attr.name`.

    return [collect_info]

# We don't have a provides field here, since we might not actually return HaskellReplCollectInfo,
# if the target doesn't have a HaskellInfo.
# TODO[GL]: try adding it once we can use required_providers.
haskell_repl_aspect = aspect(
    implementation = _haskell_repl_aspect_impl,
    attr_aspects = ["deps", "narrowed_deps"],
    required_aspect_providers = [HaskellCcLibrariesInfo],
    doc = """\
Haskell REPL aspect.

Used to implement the haskell_repl rule. Does not generate an executable REPL
by itself.
""",
)

def _repl_info(ctx):
    collect_info = _merge_HaskellReplCollectInfo([], [
        dep[HaskellReplCollectInfo]
        for dep in ctx.attr.deps
        if HaskellReplCollectInfo in dep
    ])
    from_source = [parse_pattern(ctx, pat) for pat in ctx.attr.experimental_from_source]
    from_binary = [parse_pattern(ctx, pat) for pat in ctx.attr.experimental_from_binary]
    return _create_HaskellReplInfo(from_source, from_binary, collect_info)

def _repl_info_multi(ctx):
    collect_info = _merge_HaskellReplCollectInfo([], [
        dep[HaskellReplCollectInfo]
        for dep in ctx.attr.deps
        if HaskellReplCollectInfo in dep
    ])
    from_source = [parse_pattern(ctx, pat) for pat in ctx.attr.experimental_from_source]
    from_binary = [parse_pattern(ctx, pat) for pat in ctx.attr.experimental_from_binary]
    return _create_HaskellMultiReplInfo(from_source, from_binary, collect_info)

_repl_hie_bios_common_attrs = {
    "_cc_toolchain": attr.label(
        default = Label("@rules_cc//cc:current_cc_toolchain"),
    ),
    "deps": attr.label_list(
        aspects = [
            haskell_cc_libraries_aspect,
            haskell_repl_aspect,
        ],
        doc = "List of Haskell targets to load into the REPL",
    ),
    "data": attr.label_list(
        allow_files = True,
        doc = "See [Bazel documentation](https://docs.bazel.build/versions/master/be/common-definitions.html#common.data). Only available when `collect_data = True`.",
    ),
    "experimental_from_source": attr.string_list(
        doc = """White-list of targets to load by source.

            Wild-card targets such as //... or //:all are allowed.

            The black-list takes precedence over the white-list.

            Note, this attribute will change depending on the outcome of
            https://github.com/bazelbuild/bazel/issues/7763.
            """,
        default = ["//..."],
    ),
    "experimental_from_binary": attr.string_list(
        doc = """Black-list of targets to not load by source but as packages.

            Wild-card targets such as //... or //:all are allowed.

            The black-list takes precedence over the white-list.

            Note, this attribute will change depending on the outcome of
            https://github.com/bazelbuild/bazel/issues/7763.
            """,
        default = [],
    ),
    "repl_ghci_args": attr.string_list(
        doc = "Arbitrary extra arguments to pass to GHCi. This extends `ghcopts` (previously `compiler_flags`) and `repl_ghci_args` from the toolchain. Subject to Make variable substitution.",
        default = [],
    ),
    "repl_ghci_commands": attr.string_list(
        doc = "Arbitrary extra commands to execute in GHCi.",
        default = [],
    ),
    "collect_data": attr.bool(
        doc = "Whether to collect the data runfiles from the dependencies in srcs, data and deps attributes.",
        default = True,
    ),
    "multi": attr.bool(
        doc = "Whether to use the -unit support available from GHC 9.4 onward to start a multi-repl.",
        default = False,
    ),
}

_haskell_repl = rule(
    implementation = _haskell_repl_impl,
    attrs = dict(
        _repl_hie_bios_common_attrs,
        _ghci_repl_script = attr.label(
            allow_single_file = True,
            default = Label("@rules_haskell//haskell:assets/ghci_script"),
        ),
        _ghci_repl_wrapper = attr.label(
            allow_single_file = True,
            default = Label("@rules_haskell//haskell:private/ghci_repl_wrapper.sh"),
        ),
    ),
    executable = True,
    outputs = {
        "repl": "%{name}@repl",
    },
    toolchains = use_cc_toolchain() + [
        "@rules_haskell//haskell:toolchain",
        "@rules_sh//sh/posix:toolchain_type",
    ],
    fragments = ["cpp"],
    doc = """ Build a REPL for multiple targets. """,
)

_hie_bios = rule(
    implementation = _hie_bios_impl,
    attrs =
        dict(
            _repl_hie_bios_common_attrs,
            _hie_bios_wrapper = attr.label(
                allow_single_file = True,
                default = Label("@rules_haskell//haskell:private/hie_bios_wrapper.sh"),
            ),
            _bash_runfiles = attr.label(
                default = Label("@bazel_tools//tools/bash/runfiles"),
            ),
        ),
    toolchains = use_cc_toolchain() + [
        "@rules_haskell//haskell:toolchain",
        "@rules_sh//sh/posix:toolchain_type",
    ],
    fragments = ["cpp"],
    doc = "Build an executable script that outputs flags for ide integration",
    executable = True,
)

def haskell_repl(
        name,
        deps,
        data = [],
        experimental_from_source = ["//..."],
        experimental_from_binary = [],
        repl_ghci_args = [],
        repl_ghci_commands = [],
        collect_data = True,
        multi = False,
        **kwargs):
    """Build a REPL for multiple targets.

### Examples

  ```bzl
  haskell_repl(
      name = "repl",
      deps = [
          "//lib:some_lib",
          "//exe:some_exe",
      ],
      experimental_from_source = [
          "//lib/...",
          "//exe/...",
          "//common/...",
      ],
      experimental_from_binary = [
          "//lib/vendored/...",
      ],
  )
  ```

  Collects all transitive Haskell dependencies from `deps`. Those that match
  `experimental_from_binary` or are defined in an external workspace will be
  loaded as binary packages. Those that match `experimental_from_source` and
  are defined in the local workspace will be loaded by source.

  You can call the REPL like this:

```
$ bazel run //:repl
```

### IDE Support (Experimental)

The `haskell_repl` macro also creates a runnable target `name@bios` that
generates GHCi flags for [hie-bios](https://github.com/mpickering/hie-bios)'s
`bios` cradle. You can use this for IDE support with
[ghcide](https://github.com/digital-asset/ghcide).

Given a `haskell_repl` target `//:repl` an example `.hie-bios` script could
look as follows. Please refer to the `hie-bios` documentation for further
information.

  ```shell
  #!/usr/bin/env bash
  set -euo pipefail
  bazel run //:repl@bios
  ```

    Args:
      name: A unique name for this rule.
      deps: List of Haskell targets to load into the REPL.
      data: See [Bazel documentation](https://docs.bazel.build/versions/master/be/common-definitions.html#common.data). Only available when `collect_data = True`.
      experimental_from_source: White-list of targets to load by source.
            Wild-card targets such as //... or //:all are allowed.

            The black-list takes precedence over the white-list.

            Note, this attribute will change depending on the outcome of
            https://github.com/bazelbuild/bazel/issues/7763.
      experimental_from_binary:
            Black-list of targets to not load by source but as packages.

            Wild-card targets such as //... or //:all are allowed.

            The black-list takes precedence over the white-list.

            Note, this attribute will change depending on the outcome of
            https://github.com/bazelbuild/bazel/issues/7763.
      repl_ghci_args:
            Arbitrary extra arguments to pass to GHCi. This extends `ghcopts` (previously `compiler_flags`) and `repl_ghci_args` from the toolchain. Subject to Make variable substitution.
      repl_ghci_commands:
            Arbitrary extra commands to execute in GHCi.
      collect_data:
            Whether to collect the data runfiles from the dependencies in srcs, data and deps attributes.

    Deprecated:
      hie_bios_path_prefix: Attribute has no effect (now that we output absolute paths).
"""
    if "hie_bios_path_prefix" in kwargs:
        kwargs.pop("hie_bios_path_prefix")

    _haskell_repl(
        name = name,
        deps = deps,
        data = data,
        experimental_from_source = experimental_from_source,
        experimental_from_binary = experimental_from_binary,
        repl_ghci_args = repl_ghci_args,
        repl_ghci_commands = repl_ghci_commands,
        collect_data = collect_data,
        multi = multi,
        **kwargs
    )

    hie_bios_runnable_target_name = "{}@bios".format(name)
    hie_bios_script_name = "{}-script".format(hie_bios_runnable_target_name)

    _hie_bios(
        name = hie_bios_script_name,
        deps = deps,
        data = data,
        experimental_from_source = experimental_from_source,
        experimental_from_binary = experimental_from_binary,
        repl_ghci_args = repl_ghci_args,
        repl_ghci_commands = repl_ghci_commands,
        collect_data = collect_data,
        multi = multi,
        **kwargs
    )
    sh_binary(
        name = hie_bios_runnable_target_name,
        srcs = [
            hie_bios_script_name,
        ],
        **kwargs
    )
