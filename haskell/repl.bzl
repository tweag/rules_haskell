"""Multi target Haskell REPL."""

load("@bazel_skylib//lib:paths.bzl", "paths")
load("@bazel_skylib//lib:shell.bzl", "shell")
load("@io_tweag_rules_haskell//haskell:private/context.bzl", "haskell_context", "render_env")
load(
    "@io_tweag_rules_haskell//haskell:private/path_utils.bzl",
    "link_libraries",
    "match_label",
    "parse_pattern",
    "target_unique_name",
)
load(
    "@io_tweag_rules_haskell//haskell:providers.bzl",
    "HaskellInfo",
    "HaskellLibraryInfo",
    "empty_HaskellCcInfo",
    "get_libs_for_ghc_linker",
    "merge_HaskellCcInfo",
)
load("@io_tweag_rules_haskell//haskell:private/set.bzl", "set")

HaskellReplLoadInfo = provider(
    doc = """Haskell REPL target information.

    Information to a Haskell target to load into the REPL as source.
    """,
    fields = {
        "source_files": "Set of files that contain Haskell modules.",
        "cc_dependencies": "Direct cc library dependencies. See HaskellCcInfo.",
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
        "prebuilt_dependencies": "Transitive collection of info of wired-in Haskell dependencies.",
        "transitive_cc_dependencies": "Transitive cc library dependencies. See HaskellCcInfo.",
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
        "prebuilt_dependencies": "Transitive collection of info of wired-in Haskell dependencies.",
        "transitive_cc_dependencies": "Transitive cc library dependencies. See HaskellCcInfo.",
    },
)

def _merge_HaskellReplLoadInfo(load_infos):
    source_files = set.empty()
    cc_dependencies = empty_HaskellCcInfo()
    compiler_flags = []
    repl_ghci_args = []

    for load_info in load_infos:
        set.mutable_union(source_files, load_info.source_files)
        cc_dependencies = merge_HaskellCcInfo(
            cc_dependencies,
            load_info.cc_dependencies,
        )
        compiler_flags += load_info.compiler_flags
        repl_ghci_args += load_info.repl_ghci_args

    return HaskellReplLoadInfo(
        source_files = source_files,
        cc_dependencies = cc_dependencies,
        compiler_flags = compiler_flags,
        repl_ghci_args = repl_ghci_args,
    )

def _merge_HaskellReplDepInfo(dep_infos):
    package_ids = set.empty()
    package_databases = set.empty()

    for dep_info in dep_infos:
        set.mutable_union(package_ids, dep_info.package_ids)
        set.mutable_union(package_databases, dep_info.package_databases)

    return HaskellReplDepInfo(
        package_ids = package_ids,
        package_databases = package_databases,
    )

def _create_HaskellReplCollectInfo(target, ctx):
    load_infos = {}
    dep_infos = {}

    hs_info = target[HaskellInfo]
    prebuilt_dependencies = hs_info.prebuilt_dependencies
    transitive_cc_dependencies = hs_info.transitive_cc_dependencies

    load_infos[target.label] = HaskellReplLoadInfo(
        source_files = hs_info.source_files,
        cc_dependencies = hs_info.cc_dependencies,
        compiler_flags = getattr(ctx.rule.attr, "compiler_flags", []),
        repl_ghci_args = getattr(ctx.rule.attr, "repl_ghci_args", []),
    )
    if HaskellLibraryInfo in target:
        lib_info = target[HaskellLibraryInfo]
        dep_infos[target.label] = HaskellReplDepInfo(
            package_ids = set.singleton(lib_info.package_id),
            package_databases = hs_info.package_databases,
        )

    return HaskellReplCollectInfo(
        load_infos = load_infos,
        dep_infos = dep_infos,
        prebuilt_dependencies = prebuilt_dependencies,
        transitive_cc_dependencies = transitive_cc_dependencies,
    )

def _merge_HaskellReplCollectInfo(args):
    load_infos = {}
    dep_infos = {}
    prebuilt_dependencies = set.empty()
    transitive_cc_dependencies = empty_HaskellCcInfo()
    for arg in args:
        load_infos.update(arg.load_infos)
        dep_infos.update(arg.dep_infos)
        set.mutable_union(
            prebuilt_dependencies,
            arg.prebuilt_dependencies,
        )
        transitive_cc_dependencies = merge_HaskellCcInfo(
            transitive_cc_dependencies,
            arg.transitive_cc_dependencies,
        )

    return HaskellReplCollectInfo(
        load_infos = load_infos,
        dep_infos = dep_infos,
        prebuilt_dependencies = prebuilt_dependencies,
        transitive_cc_dependencies = transitive_cc_dependencies,
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

    # Collect all packages to load by source.
    load_info = _merge_HaskellReplLoadInfo([
        load_info
        for (lbl, load_info) in collect_info.load_infos.items()
        if _load_as_source(from_source, from_binary, lbl)
    ])

    # Collect all packages to load as binary packages.
    dep_info = _merge_HaskellReplDepInfo([
        dep_info
        for (lbl, dep_info) in collect_info.dep_infos.items()
        if not _load_as_source(from_source, from_binary, lbl)
    ])

    return HaskellReplInfo(
        load_info = load_info,
        dep_info = dep_info,
        prebuilt_dependencies = collect_info.prebuilt_dependencies,
        transitive_cc_dependencies = collect_info.transitive_cc_dependencies,
    )

def _create_repl(hs, ctx, repl_info, output):
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
    args = ["-package", "base", "-package", "directory"]

    # Load prebuilt dependencies (-package)
    for dep in set.to_list(repl_info.prebuilt_dependencies):
        args.extend(["-package", dep.package])

    # Load built dependencies (-package-id, -package-db)
    for package_id in set.to_list(repl_info.dep_info.package_ids):
        args.extend(["-package-id", package_id])
    for package_cache in set.to_list(repl_info.dep_info.package_databases):
        args.extend([
            "-package-db",
            paths.join("$RULES_HASKELL_EXEC_ROOT", package_cache.dirname),
        ])

    # Load C library dependencies
    link_ctx = repl_info.load_info.cc_dependencies.dynamic_linking
    libs_to_link = link_ctx.dynamic_libraries_for_runtime.to_list()

    # External C libraries that we need to make available to the REPL.
    libraries = link_libraries(libs_to_link, args)

    # Transitive library dependencies to have in runfiles.
    (library_deps, ld_library_deps, ghc_env) = get_libs_for_ghc_linker(
        hs,
        repl_info.transitive_cc_dependencies,
        path_prefix = "$RULES_HASKELL_EXEC_ROOT",
    )
    library_path = [paths.dirname(lib.path) for lib in library_deps]
    ld_library_path = [paths.dirname(lib.path) for lib in ld_library_deps]

    # Load source files
    # Force loading by source with `:add *...`.
    # See https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html#ghci-cmd-:add
    add_sources = [
        "*" + f.path
        for f in set.to_list(repl_info.load_info.source_files)
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
            "{ENV}": render_env(ghc_env),
            "{TOOL}": hs.tools.ghci.path,
            "{ARGS}": " ".join(
                args + [
                    shell.quote(a)
                    for a in quote_args
                ],
            ),
        },
    )

    extra_inputs = [
        hs.tools.ghci,
        ghci_repl_script,
    ]
    extra_inputs.extend(set.to_list(repl_info.load_info.source_files))
    extra_inputs.extend(set.to_list(repl_info.dep_info.package_databases))
    extra_inputs.extend(library_deps)
    extra_inputs.extend(ld_library_deps)
    return [DefaultInfo(
        executable = output,
        runfiles = ctx.runfiles(
            files = extra_inputs,
            collect_data = ctx.attr.collect_data,
        ),
    )]

def _haskell_repl_aspect_impl(target, ctx):
    if not HaskellInfo in target:
        return []

    target_info = _create_HaskellReplCollectInfo(target, ctx)
    deps_infos = [
        dep[HaskellReplCollectInfo]
        for dep in ctx.rule.attr.deps
        if HaskellReplCollectInfo in dep
    ]
    collect_info = _merge_HaskellReplCollectInfo([target_info] + deps_infos)

    # This aspect currently does not generate an executable REPL script by
    # itself. This could be extended in future. Note, to that end it's
    # necessary to construct a Haskell context without `ctx.attr.name`.

    return [collect_info]

haskell_repl_aspect = aspect(
    implementation = _haskell_repl_aspect_impl,
    attr_aspects = ["deps"],
)
"""
Haskell REPL aspect.

Used to implement the haskell_repl rule. Does not generate an executable REPL
by itself.
"""

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
    return _create_repl(hs, ctx, repl_info, ctx.outputs.repl)

haskell_repl = rule(
    implementation = _haskell_repl_impl,
    attrs = {
        "_ghci_repl_script": attr.label(
            allow_single_file = True,
            default = Label("@io_tweag_rules_haskell//haskell:assets/ghci_script"),
        ),
        "_ghci_repl_wrapper": attr.label(
            allow_single_file = True,
            default = Label("@io_tweag_rules_haskell//haskell:private/ghci_repl_wrapper.sh"),
        ),
        "deps": attr.label_list(
            aspects = [haskell_repl_aspect],
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
    toolchains = ["@io_tweag_rules_haskell//haskell:toolchain"],
)
"""Build a REPL for multiple targets.

Example:
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

"""
