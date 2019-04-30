"""Linting"""

load(
    "@io_tweag_rules_haskell//haskell:providers.bzl",
    "HaskellInfo",
    "HaskellLibraryInfo",
    "HaskellLintInfo",
)
load(":private/context.bzl", "haskell_context", "render_env")
load(":private/packages.bzl", "expose_packages", "pkg_info_to_compile_flags")
load(
    ":private/path_utils.bzl",
    "target_unique_name",
)
load(":providers.bzl", "get_libs_for_ghc_linker")
load(":private/set.bzl", "set")

def _collect_lint_logs(deps):
    lint_logs = set.empty()
    for dep in deps:
        if HaskellLintInfo in dep:
            set.mutable_union(lint_logs, dep[HaskellLintInfo].outputs)
    return lint_logs

def _haskell_lint_rule_impl(ctx):
    return [DefaultInfo(
        files = set.to_depset(_collect_lint_logs(ctx.attr.deps)),
    )]

def _haskell_lint_aspect_impl(target, ctx):
    hs = haskell_context(ctx, ctx.rule.attr)

    if HaskellInfo not in target:
        return []

    hs_info = target[HaskellInfo]
    lib_info = target[HaskellLibraryInfo] if HaskellLibraryInfo in target else None

    args = ctx.actions.args()

    args.add_all([
        "-O0",
        "-v0",
        "-fno-code",
        "-Wall",
        "-Werror",
        "-Wcompat",
        "-Wincomplete-record-updates",
        "-Wincomplete-uni-patterns",
        "-Wredundant-constraints",
        "-Wnoncanonical-monad-instances",
        "--make",
    ])

    args.add_all(pkg_info_to_compile_flags(expose_packages(
        hs_info,
        lib_info,
        use_direct = False,
        use_my_pkg_id = None,
        custom_package_databases = None,
        version = ctx.rule.attr.version,
    )))

    sources = set.to_list(hs_info.source_files)

    args.add_all(sources)

    lint_log = ctx.actions.declare_file(
        target_unique_name(hs, "lint-log"),
    )

    # Transitive library dependencies for runtime.
    (library_deps, ld_library_deps, _ghc_env) = get_libs_for_ghc_linker(
        hs,
        hs_info.transitive_cc_dependencies,
    )

    ctx.actions.run_shell(
        inputs = depset(transitive = [
            depset(sources),
            set.to_depset(hs_info.package_databases),
            set.to_depset(hs_info.interface_dirs),
            set.to_depset(hs_info.dynamic_libraries),
            depset(library_deps),
            depset(ld_library_deps),
            depset([hs.tools.ghc]),
        ]),
        outputs = [lint_log],
        mnemonic = "HaskellLint",
        progress_message = "HaskellLint {}".format(ctx.label),
        command = """
        {env}
        {ghc} "$@" > {output} 2>&1 || rc=$? && cat {output} && exit $rc
        """.format(
            ghc = hs.tools.ghc.path,
            output = lint_log.path,
            # XXX Workaround
            # https://github.com/bazelbuild/bazel/issues/5980.
            env = render_env(hs.env),
        ),
        arguments = [args],
        use_default_shell_env = True,
    )

    lint_info = HaskellLintInfo(outputs = set.singleton(lint_log))
    output_files = OutputGroupInfo(default = [lint_log])

    return [lint_info, output_files]

haskell_lint_aspect = aspect(
    _haskell_lint_aspect_impl,
    attr_aspects = ["deps"],
    toolchains = ["@io_tweag_rules_haskell//haskell:toolchain"],
)

haskell_lint = rule(
    _haskell_lint_rule_impl,
    attrs = {
        "deps": attr.label_list(
            aspects = [haskell_lint_aspect],
            doc = "List of Haskell targets to lint.",
        ),
    },
    toolchains = ["@io_tweag_rules_haskell//haskell:toolchain"],
)
"""Check source code of targets in `deps` using a restrictive set of GHC
flags.

The following flags will be used:

* `-Wall`
* `-Werror`
* `-Wcompat`
* `-Wincomplete-record-updates`
* `-Wincomplete-uni-patterns`
* `-Wredundant-constraints`
"""
