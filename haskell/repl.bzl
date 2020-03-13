"""Multi target Haskell REPL."""

load("@bazel_skylib//lib:dicts.bzl", "dicts")
load("@bazel_skylib//lib:paths.bzl", "paths")
load("@bazel_skylib//lib:shell.bzl", "shell")
load(":cc.bzl", "ghc_cc_program_args")
load(":private/context.bzl", "haskell_context", "render_env")
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
    "haskell_cc_libraries_aspect",
    "link_libraries",
    "merge_HaskellCcLibrariesInfo",
)
load(":private/set.bzl", "set")

HaskellReplLoadInfo = provider(
    doc = """Haskell REPL target information.

    Information to a Haskell target to load into the REPL as source.
    """,
    fields = {
        "source_files": "Set of files that contain Haskell modules.",
        "import_dirs": "Set of Haskell import directories.",
        "cc_libraries_info": "HaskellCcLibrariesInfo of transitive C dependencies.",
        "cc_info": "CcInfo of transitive C dependencies.",
        "compiler_flags": "Flags to pass to the Haskell compiler.",
        "repl_ghci_args": "Arbitrary extra arguments to pass to GHCi. This extends `compiler_flags` and `repl_ghci_args` from the toolchain",
    },
)

HaskellReplDepInfo = provider(
    doc = """Haskell REPL dependency information.

    Information to a Haskell target to load into the REPL as a built package.
    """,
    fields = {
        "package_ids": "Set of workspace unique package identifiers.",
        "package_databases": "Set of package cache files.",
        "cc_libraries_info": "HaskellCcLibrariesInfo of transitive C dependencies.",
        "cc_info": "CcInfo of the package itself (includes its transitive dependencies).",
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

def _merge_HaskellReplLoadInfo(load_infos):
    source_files = depset()
    import_dirs = depset()
    cc_libraries_infos = []
    cc_infos = []
    compiler_flags = []
    repl_ghci_args = []

    for load_info in load_infos:
        source_files = depset(transitive = [source_files, load_info.source_files])
        import_dirs = depset(transitive = [import_dirs, load_info.import_dirs])
        cc_libraries_infos.append(load_info.cc_libraries_info)
        cc_infos.append(load_info.cc_info)
        compiler_flags += load_info.compiler_flags
        repl_ghci_args += load_info.repl_ghci_args

    return HaskellReplLoadInfo(
        source_files = source_files,
        import_dirs = import_dirs,
        cc_libraries_info = merge_HaskellCcLibrariesInfo(infos = cc_libraries_infos),
        cc_info = cc_common.merge_cc_infos(cc_infos = cc_infos),
        compiler_flags = compiler_flags,
        repl_ghci_args = repl_ghci_args,
    )

def _merge_HaskellReplDepInfo(dep_infos):
    package_ids = []
    package_databases = depset()
    cc_libraries_infos = []
    cc_infos = []

    for dep_info in dep_infos:
        package_ids += dep_info.package_ids
        package_databases = depset(transitive = [package_databases, dep_info.package_databases])
        cc_libraries_infos.append(dep_info.cc_libraries_info)
        cc_infos.append(dep_info.cc_info)

    return HaskellReplDepInfo(
        package_ids = package_ids,
        package_databases = package_databases,
        cc_libraries_info = merge_HaskellCcLibrariesInfo(infos = cc_libraries_infos),
        cc_info = cc_common.merge_cc_infos(cc_infos = cc_infos),
    )

def _create_HaskellReplCollectInfo(target, ctx):
    load_infos = {}
    dep_infos = {}

    hs_info = target[HaskellInfo]

    if not HaskellToolchainLibraryInfo in target:
        load_infos[target.label] = HaskellReplLoadInfo(
            source_files = hs_info.source_files,
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
            compiler_flags = getattr(ctx.rule.attr, "compiler_flags", []),
            repl_ghci_args = getattr(ctx.rule.attr, "repl_ghci_args", []),
        )
    if HaskellLibraryInfo in target:
        lib_info = target[HaskellLibraryInfo]
        dep_infos[target.label] = HaskellReplDepInfo(
            package_ids = all_package_ids(lib_info),
            package_databases = hs_info.package_databases,
            cc_libraries_info = target[HaskellCcLibrariesInfo],
            cc_info = target[CcInfo],
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

def _compiler_flags_and_inputs(hs, repl_info, path_prefix = ""):
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

    Args:
      hs: Haskell context.
      args: list of string, output, the arguments to extend.
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
    all_libraries = cc_info.linking_context.libraries_to_link.to_list()
    cc_libraries = get_cc_libraries(cc_libraries_info, all_libraries)
    link_libraries(
        get_ghci_library_files(hs, cc_libraries_info, cc_libraries),
        args,
    )

    args.extend(ghc_cc_program_args(
        paths.join(path_prefix, hs.toolchain.cc_wrapper.executable.path),
    ))

    # Add import directories
    for import_dir in repl_info.load_info.import_dirs.to_list():
        args.append("-i" + (import_dir if import_dir else "."))

    inputs = depset(transitive = [
        repl_info.load_info.source_files,
        repl_info.dep_info.package_databases,
        depset(get_ghci_library_files(hs, cc_libraries_info, all_libraries)),
        depset([hs.toolchain.locale_archive] if hs.toolchain.locale_archive else []),
    ])

    return (args, inputs)

def _create_repl(hs, posix, ctx, repl_info, output):
    """Build a multi target REPL.

    Args:
      hs: Haskell context.
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

    compiler_flags, inputs = _compiler_flags_and_inputs(hs, repl_info, path_prefix = "$RULES_HASKELL_EXEC_ROOT")
    args.extend(compiler_flags)

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
    quote_args = (
        hs.toolchain.compiler_flags +
        repl_info.load_info.compiler_flags +
        hs.toolchain.repl_ghci_args +
        repl_info.load_info.repl_ghci_args +
        ctx.attr.repl_ghci_args
    )

    hs.actions.expand_template(
        template = ctx.file._ghci_repl_wrapper,
        output = output,
        is_executable = True,
        substitutions = {
            "%{ENV}": render_env(hs.env),
            "%{TOOL}": hs.tools.ghci.path,
            "%{ARGS}": "(" + " ".join(
                args + [
                    shell.quote(a)
                    for a in quote_args
                ],
            ) + ")",
        },
    )

    return [DefaultInfo(
        executable = output,
        runfiles = ctx.runfiles(
            files = [
                hs.tools.ghci,
                ghci_repl_script,
            ],
            transitive_files = inputs,
            collect_data = ctx.attr.collect_data,
        ).merge(
            hs.toolchain.cc_wrapper.runfiles,
        ),
    )]

def _create_hie_bios(hs, posix, ctx, repl_info):
    """Build a hie-bios argument file.

    Args:
      hs: Haskell context.
      ctx: Rule context.
      repl_info: HaskellReplInfo provider.
      output: The output for the executable REPL script.

    Returns:
      List of providers:
        OutputGroupInfo provider for the hie-bios argument file.
    """
    args, inputs = _compiler_flags_and_inputs(hs, repl_info)
    args.extend(hs.toolchain.compiler_flags)
    args.extend(repl_info.load_info.compiler_flags)

    args_file = ctx.actions.declare_file(".%s.hie-bios" % ctx.label.name)
    args_link = ctx.actions.declare_file("%s@hie-bios" % ctx.label.name)
    ctx.actions.write(args_file, "\n".join(args))
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
    posix = ctx.toolchains["@rules_sh//sh/posix:toolchain_type"]
    return _create_repl(hs, posix, ctx, repl_info, ctx.outputs.repl) + \
           _create_hie_bios(hs, posix, ctx, repl_info)

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
        "deps": attr.label_list(
            aspects = [
                haskell_cc_libraries_aspect,
                haskell_repl_aspect,
            ],
            doc = "List of Haskell targets to load into the REPL",
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
            doc = "Arbitrary extra arguments to pass to GHCi. This extends `compiler_flags` and `repl_ghci_args` from the toolchain",
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
    },
    executable = True,
    outputs = {
        "repl": "%{name}@repl",
    },
    toolchains = [
        "@rules_haskell//haskell:toolchain",
        "@rules_sh//sh/posix:toolchain_type",
    ],
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
