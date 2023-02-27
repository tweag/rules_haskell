"""Multi target Haskell REPL."""

load("@bazel_skylib//lib:dicts.bzl", "dicts")
load("@bazel_skylib//lib:paths.bzl", "paths")
load("@bazel_skylib//lib:shell.bzl", "shell")
load("@rules_cc//cc:find_cc_toolchain.bzl", "find_cc_toolchain")
load(":cc.bzl", "ghc_cc_program_args")
load(":private/context.bzl", "haskell_context", "render_env")
load(":private/expansions.bzl", "expand_make_variables")
load(
    ":private/path_utils.bzl",
    "match_label",
    "parse_pattern",
    "target_unique_name",
)
load(
    ":providers.bzl",
    "HaskellCcLibrariesInfo",
    "HaskellInfo",
    "HaskellLibraryInfo",
    "HaskellToolchainLibraryInfo",
    "all_package_ids",
)
load(
    ":private/cc_libraries.bzl",
    "deps_HaskellCcLibrariesInfo",
    "get_cc_libraries",
    "get_ghci_library_files",
    "get_library_files",
    "haskell_cc_libraries_aspect",
    "link_libraries",
    "merge_HaskellCcLibrariesInfo",
)
load(":private/java.bzl", "java_interop_info")
load(":private/set.bzl", "set")
load("@bazel_skylib//lib:sets.bzl", "sets")

HaskellReplLoadInfo = provider(
    doc = """Haskell REPL target information.

    Information to a Haskell target to load into the REPL as source.
    """,
    fields = {
        "source_files": "Depset of files that contain Haskell modules.",
        "boot_files": "Depset of Haskell boot files.",
        "module_names": "Depset of Haskell module names to load.",
        "import_dirs": "Depset of Haskell import directories.",
        "cc_libraries_info": "HaskellCcLibrariesInfo of transitive C dependencies.",
        "cc_info": "CcInfo of transitive C dependencies.",
        "compiler_flags": "Flags to pass to the Haskell compiler.",
        "repl_ghci_args": "Arbitrary extra arguments to pass to GHCi. This extends `compiler_flags` and `repl_ghci_args` from the toolchain",
        "data_runfiles": "Runtime data dependencies of this target, i.e. the files and runfiles of the `data` attribute.",
        "java_deps": "depset of Files to jars needed for building.",
    },
)

HaskellReplDepInfo = provider(
    doc = """Haskell REPL dependency information.

    Information to a Haskell target to load into the REPL as a built package.
    """,
    fields = {
        "package_ids": "Set of workspace unique package identifiers.",
        "package_databases": "Set of package cache files.",
        "interface_dirs": "Set of interface dirs for all the dependencies",
        "cc_libraries_info": "HaskellCcLibrariesInfo of transitive C dependencies.",
        "cc_info": "CcInfo of the package itself (includes its transitive dependencies).",
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
        compiler_flags += load_info.compiler_flags
        repl_ghci_args += load_info.repl_ghci_args
        data_runfiles.append(load_info.data_runfiles)
        java_deps.append(load_info.java_deps)

    return HaskellReplLoadInfo(
        source_files = source_files,
        boot_files = boot_files,
        module_names = module_names,
        import_dirs = import_dirs,
        cc_libraries_info = merge_HaskellCcLibrariesInfo(infos = cc_libraries_infos),
        cc_info = cc_common.merge_cc_infos(cc_infos = cc_infos),
        compiler_flags = compiler_flags,
        repl_ghci_args = repl_ghci_args,
        data_runfiles = _merge_runfiles(data_runfiles),
        java_deps = depset(transitive = java_deps),
    )

def _merge_HaskellReplDepInfo(dep_infos):
    package_ids = []
    package_databases = depset()
    interface_dirs = depset()
    cc_libraries_infos = []
    cc_infos = []
    runfiles = []

    for dep_info in dep_infos:
        package_ids += dep_info.package_ids
        package_databases = depset(transitive = [package_databases, dep_info.package_databases])
        interface_dirs = depset(transitive = [interface_dirs, dep_info.interface_dirs])
        cc_libraries_infos.append(dep_info.cc_libraries_info)
        cc_infos.append(dep_info.cc_info)
        runfiles.append(dep_info.runfiles)

    return HaskellReplDepInfo(
        package_ids = package_ids,
        package_databases = package_databases,
        interface_dirs = interface_dirs,
        cc_libraries_info = merge_HaskellCcLibrariesInfo(infos = cc_libraries_infos),
        cc_info = cc_common.merge_cc_infos(cc_infos = cc_infos),
        runfiles = _merge_runfiles(runfiles),
    )

def _create_HaskellReplCollectInfo(target, ctx):
    load_infos = {}
    dep_infos = {}

    hs_info = target[HaskellInfo]

    if not HaskellToolchainLibraryInfo in target:
        java_deps_list = []

        if hasattr(ctx.rule.attr, "deps"):
            java_deps_list += [java_interop_info(ctx.rule.attr.deps).inputs]

        # TODO[GL]: add tests for the java deps in narrowed_deps
        if hasattr(ctx.rule.attr, "narrowed_deps"):
            java_deps_list += [java_interop_info(ctx.rule.attr.narrowed_deps).inputs]

        java_deps = depset(transitive = java_deps_list)

        # TODO[GL]: add tests for CcInfo deps in narrowed_deps
        ccInfoDeps = [
            dep
            for dep in getattr(ctx.rule.attr, "deps", []) + getattr(ctx.rule.attr, "narrowed_deps", [])
            if CcInfo in dep and not HaskellInfo in dep
        ]
        load_infos[target.label] = HaskellReplLoadInfo(
            source_files = hs_info.source_files,
            boot_files = hs_info.boot_files,
            module_names = hs_info.module_names,
            import_dirs = set.to_depset(hs_info.import_dirs),
            cc_libraries_info = deps_HaskellCcLibrariesInfo(ccInfoDeps),
            cc_info = cc_common.merge_cc_infos(cc_infos = [
                # Collect pure C library dependencies, no Haskell dependencies.
                dep[CcInfo]
                for dep in ccInfoDeps
            ]),
            compiler_flags = hs_info.user_compile_flags,
            repl_ghci_args = hs_info.user_repl_flags,
            data_runfiles = _data_runfiles(ctx, ctx.rule, "data"),
            java_deps = java_deps,
        )
    if HaskellLibraryInfo in target:
        lib_info = target[HaskellLibraryInfo]
        dep_infos[target.label] = HaskellReplDepInfo(
            package_ids = all_package_ids(lib_info),
            package_databases = hs_info.package_databases,
            interface_dirs = hs_info.interface_dirs,
            cc_libraries_info = target[HaskellCcLibrariesInfo],
            cc_info = target[CcInfo],
            runfiles = target[DefaultInfo].default_runfiles,
        )

    return HaskellReplCollectInfo(
        load_infos = load_infos,
        dep_infos = dep_infos,
    )

def _merge_HaskellReplCollectInfo(args):
    load_infos = {}
    dep_infos = {}
    for arg in args:
        load_infos.update(arg.load_infos)
        dep_infos.update(arg.dep_infos)

    return HaskellReplCollectInfo(
        load_infos = load_infos,
        dep_infos = dep_infos,
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

def _concat(lists):
    return [item for l in lists for item in l]

def _compiler_flags_and_inputs(hs, cc, repl_info, get_dirname, static = False):
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
    for package_id in repl_info.dep_info.package_ids:
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
    all_libraries = [lib for li in cc_info.linking_context.linker_inputs.to_list() for lib in li.libraries]
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
            "%{TOOL}": hs.tools.ghci.path,
            "%{OUTPUT}": paths.dirname(output.path),
            "%{ARGS}": "(" + " ".join(
                args + [
                    shell.quote(a)
                    for a in quote_args
                ],
            ) + ")",
        },
    )

    runfiles = [
        ctx.runfiles(
            files = [
                hs.tools.ghci,
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
    args, inputs = _compiler_flags_and_inputs(
        hs,
        cc,
        repl_info,
        static = True,
        get_dirname = lambda p: "$(dirname {})".format(_rlocation(ctx, p)),
    )
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

    # Add import directories.
    # Note, src_strip_prefix is deprecated. However, for now ghcide depends on
    # `-i` flags to find source files to modules.
    import_dirs = [paths.join("$RULES_HASKELL_EXEC_ROOT", dir) for dir in repl_info.load_info.import_dirs.to_list()]
    for import_dir in import_dirs:
        args.append("-i" + (import_dir or "."))

    # List modules (Targets) covered by this cradle.
    args.extend([_rlocation(ctx, f) for f in repl_info.load_info.source_files.to_list()])

    # List boot files
    args.extend([_rlocation(ctx, f) for f in repl_info.load_info.boot_files.to_list()])

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
        },
    )
    return [DefaultInfo(
        executable = hie_bios_script,
        runfiles = runfiles,
    )]

def _haskell_repl_aspect_impl(target, ctx):
    # TODO[GL]: try removing this and using required_providers, once we're on a newer version of bazel
    if HaskellInfo not in target:
        return []

    target_info = _create_HaskellReplCollectInfo(target, ctx)

    deps_infos = [
        dep[HaskellReplCollectInfo]
        for deps_field_name in ["deps", "narrowed_deps"]
        for dep in getattr(ctx.rule.attr, deps_field_name, [])
        if HaskellReplCollectInfo in dep
    ]

    collect_info = _merge_HaskellReplCollectInfo([target_info] + deps_infos)

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
    collect_info = _merge_HaskellReplCollectInfo([
        dep[HaskellReplCollectInfo]
        for dep in ctx.attr.deps
        if HaskellReplCollectInfo in dep
    ])
    from_source = [parse_pattern(ctx, pat) for pat in ctx.attr.experimental_from_source]
    from_binary = [parse_pattern(ctx, pat) for pat in ctx.attr.experimental_from_binary]
    return _create_HaskellReplInfo(from_source, from_binary, collect_info)

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
    toolchains = [
        "@rules_haskell//haskell:toolchain",
        "@rules_cc//cc:toolchain_type",
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
    toolchains = [
        "@rules_haskell//haskell:toolchain",
        "@rules_cc//cc:toolchain_type",
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
        **kwargs
    )

    native.sh_binary(
        name = hie_bios_runnable_target_name,
        srcs = [
            hie_bios_script_name,
        ],
        **kwargs
    )
