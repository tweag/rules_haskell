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
    "ln",
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

HaskellReplLoadInfo = provider(
    doc = """Haskell REPL target information.

    Information to a Haskell target to load into the REPL as source.
    """,
    fields = {
        "source_files": "Depset of files that contain Haskell modules.",
        "boot_files": "Depset of Haskell boot files.",
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
        if hasattr(ctx.rule.attr, "deps"):
            java_deps = java_interop_info(ctx.rule.attr.deps).inputs
        else:
            java_deps = depset()
        load_infos[target.label] = HaskellReplLoadInfo(
            source_files = hs_info.source_files,
            boot_files = hs_info.boot_files,
            import_dirs = set.to_depset(hs_info.import_dirs),
            cc_libraries_info = deps_HaskellCcLibrariesInfo([
                dep
                for dep in getattr(ctx.rule.attr, "deps", [])
                if CcInfo in dep and not HaskellInfo in dep
            ]),
            cc_info = cc_common.merge_cc_infos(cc_infos = [
                # Collect pure C library dependencies, no Haskell dependencies.
                dep[CcInfo]
                for dep in getattr(ctx.rule.attr, "deps", [])
                if CcInfo in dep and not HaskellInfo in dep
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

def _compiler_flags_and_inputs(hs, cc, repl_info, static = False, path_prefix = ""):
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
      path_prefix: string, optional, Prefix for package db paths.

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
        args.extend(["-package-db", paths.join(path_prefix, package_cache.dirname)])

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
    link_libraries(cc_library_files, args, path_prefix = path_prefix)

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

def _create_repl(hs, cc, posix, ctx, repl_info, output):
    """Build a multi target REPL.

    Args:
      hs: Haskell context.
      cc: CcToolchainInfo.
      posix: POSIX toolchain.
      ctx: Rule context.
      repl_info: HaskellReplInfo provider.
      output: The output for the executable REPL script.

    Returns:
      List of providers:
        DefaultInfo provider for the executable REPL script.

    """

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
        path_prefix = "$RULES_HASKELL_EXEC_ROOT",
    )
    args.extend(compiler_flags)
    cc_path = paths.join(
        "$RULES_HASKELL_EXEC_ROOT",
        hs.toolchain.cc_wrapper.executable.path,
    )
    args.extend(['"{}"'.format(arg) for arg in ghc_cc_program_args(hs, cc_path)])

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

def _create_hie_bios(hs, cc, posix, ctx, repl_info, path_prefix):
    """Build a hie-bios argument file.

    Args:
      hs: Haskell context.
      cc: CcToolchainInfo.
      posix: POSIX toolchain.
      ctx: Rule context.
      repl_info: HaskellReplInfo provider.
      output: The output for the executable REPL script.

    Returns:
      List of providers:
        OutputGroupInfo provider for the hie-bios argument file.
    """
    path_prefix = paths.join("", *path_prefix)
    args, inputs = _compiler_flags_and_inputs(hs, cc, repl_info, path_prefix = path_prefix, static = True)
    cc_path = paths.join(path_prefix, hs.toolchain.cc_wrapper.executable.path)
    args.extend(ghc_cc_program_args(hs, cc_path))
    args.extend(hs.toolchain.ghcopts)
    args.extend(repl_info.load_info.compiler_flags)

    # Add import directories.
    # Note, src_strip_prefix is deprecated. However, for now ghcide depends on
    # `-i` flags to find source files to modules.
    for import_dir in repl_info.load_info.import_dirs.to_list():
        args.append("-i" + (paths.join(path_prefix, import_dir) or "."))

    # List modules (Targets) covered by this cradle.
    args.extend([paths.join(path_prefix, f.path) for f in repl_info.load_info.source_files.to_list()])

    # List boot files
    args.extend([f.path for f in repl_info.load_info.boot_files.to_list()])

    args_file = ctx.actions.declare_file(".%s.hie-bios" % ctx.label.name)
    args_link = ctx.actions.declare_file("%s@hie-bios" % ctx.label.name)
    ctx.actions.write(args_file, "\n".join(args) + "\n")
    ln(hs, posix, args_file, args_link, extra_inputs = inputs)
    return [OutputGroupInfo(hie_bios = [args_link])]

def _haskell_repl_aspect_impl(target, ctx):
    if HaskellInfo not in target:
        return []

    target_info = _create_HaskellReplCollectInfo(target, ctx)
    if hasattr(ctx.rule.attr, "deps"):
        deps_infos = [
            dep[HaskellReplCollectInfo]
            for dep in ctx.rule.attr.deps
            if HaskellReplCollectInfo in dep
        ]
    else:
        deps_infos = []
    collect_info = _merge_HaskellReplCollectInfo([target_info] + deps_infos)

    # This aspect currently does not generate an executable REPL script by
    # itself. This could be extended in future. Note, to that end it's
    # necessary to construct a Haskell context without `ctx.attr.name`.

    return [collect_info]

haskell_repl_aspect = aspect(
    implementation = _haskell_repl_aspect_impl,
    attr_aspects = ["deps"],
    required_aspect_providers = [HaskellCcLibrariesInfo],
    doc = """\
Haskell REPL aspect.

Used to implement the haskell_repl rule. Does not generate an executable REPL
by itself.
""",
)

def _haskell_repl_impl(ctx):
    collect_info = _merge_HaskellReplCollectInfo([
        dep[HaskellReplCollectInfo]
        for dep in ctx.attr.deps
        if HaskellReplCollectInfo in dep
    ])
    from_source = [parse_pattern(ctx, pat) for pat in ctx.attr.experimental_from_source]
    from_binary = [parse_pattern(ctx, pat) for pat in ctx.attr.experimental_from_binary]
    repl_info = _create_HaskellReplInfo(from_source, from_binary, collect_info)
    hs = haskell_context(ctx)
    cc = find_cc_toolchain(ctx)
    posix = ctx.toolchains["@rules_sh//sh/posix:toolchain_type"]
    return _create_repl(hs, cc, posix, ctx, repl_info, ctx.outputs.repl) + \
           _create_hie_bios(hs, cc, posix, ctx, repl_info, ctx.attr.hie_bios_path_prefix)

haskell_repl = rule(
    implementation = _haskell_repl_impl,
    attrs = {
        "_ghci_repl_script": attr.label(
            allow_single_file = True,
            default = Label("@rules_haskell//haskell:assets/ghci_script"),
        ),
        "_ghci_repl_wrapper": attr.label(
            allow_single_file = True,
            default = Label("@rules_haskell//haskell:private/ghci_repl_wrapper.sh"),
        ),
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
        "hie_bios_path_prefix": attr.string_list(
            doc = """Path prefix for hie-bios paths. The elements of the list are joined together to build the path.
                   See [IDE support](#ide-support-experimental).""",
            default = [],
        ),
    },
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
    doc = """\
Build a REPL for multiple targets.

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

`haskell_repl` targets provide the `hie_bios` output group to optionally
generate GHCi flags for [hie-bios](https://github.com/mpickering/hie-bios)'s
`bios` cradle. You can use this for IDE support with
[ghcide](https://github.com/digital-asset/ghcide).

Given a `haskell_repl` target `//:repl` an example `.hie-bios` script could
look as follows. Please refer to the `hie-bios` documentation for further
information.

  ```shell
  #!/usr/bin/env bash
  set -euo pipefail
  bazel build //:repl --output_groups=hie_bios
  cat bazel-bin/repl@hie-bios >"$HIE_BIOS_OUTPUT"
  ```
""",
)
