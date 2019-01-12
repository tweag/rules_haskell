"""Linting"""

load(
    "@io_tweag_rules_haskell//haskell:private/providers.bzl",
    "HaskellBinaryInfo",
    "HaskellBuildInfo",
    "HaskellLibraryInfo",
    "HaskellLintInfo",
)
load(":private/context.bzl", "haskell_context")
load(":private/packages.bzl", "expose_packages")
load(
    ":private/path_utils.bzl",
    "target_unique_name",
)
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

    if HaskellBuildInfo not in target:
        return []

    build_info = target[HaskellBuildInfo]
    lib_info = target[HaskellLibraryInfo] if HaskellLibraryInfo in target else None
    bin_info = target[HaskellBinaryInfo] if HaskellBinaryInfo in target else None

    args = ctx.actions.args()

    args.add([
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

    args.add(expose_packages(
        build_info,
        lib_info,
        use_direct = False,
        use_my_pkg_id = None,
        custom_package_caches = None,
        version = ctx.rule.attr.version,
    ))

    sources = set.to_list(
        lib_info.source_files if lib_info != None else bin_info.source_files,
    )

    args.add(sources)

    lint_log = ctx.actions.declare_file(
        target_unique_name(hs, "lint-log"),
    )

    ctx.actions.run_shell(
        inputs = depset(transitive = [
            depset(sources),
            set.to_depset(build_info.package_confs),
            set.to_depset(build_info.package_caches),
            set.to_depset(build_info.interface_dirs),
            set.to_depset(build_info.dynamic_libraries),
            depset([e.mangled_lib for e in set.to_list(build_info.external_libraries)]),
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
            env = "\n".join([
                "export {}={}".format(k, v)
                for k, v in hs.env.items()
            ]),
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
