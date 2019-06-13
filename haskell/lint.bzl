"""Lint Haskell sources using hlint."""

HaskellHLintInfo = provider(
    doc = "Provider that collects files produced by hlint",
    fields = {
        "outputs": "depset of hlint log files.",
    },
)

def _collect_hlint_logs(deps):
    return depset(transitive = [
        dep[HaskellHLintInfo].outputs
        for dep in deps
        if HaskellHLintInfo in dep
    ])

def _haskell_lint_aspect_impl(target, ctx):
    hlint_toolchain = ctx.toolchains["@io_tweag_rules_haskell//haskell/lint:toolchain"]
    hint = hlint_toolchain.hint

    inputFiles = []
    inputPaths = []
    if hasattr(ctx.rule.attr, "srcs"):
        for src in ctx.rule.attr.srcs:
            for f in src.files:
                # We want to only do native Haskell source files, which
                # seems to involve ignoring these generated paths
                # (the f.is_source almost always returns True)
                if all([
                    f.path.endswith(".hs"),
                    f.path.startswith("external/") == False,
                    f.path.startswith("bazel-out/") == False,
                    f.path.startswith("nix/") == False,
                ]):
                    inputFiles.append(f)
                    inputPaths.append(f.path)

    if len(inputFiles) == 0:
        return []
    output = ctx.actions.declare_file(target.label.name + "-hlint.html")
    args = ctx.actions.args()
    if hint:
        args.add(hint, format = "--hint=%s")
    args.add_all(inputPaths)
    args.add(output, format = "--report=%s")
    args.add("--verbose")

    ctx.actions.run(
        inputs = ([hint] if hint else []) + inputFiles,
        outputs = [output],
        mnemonic = "HaskellHLint",
        progress_message = "HaskellHLint {}".format(ctx.label),
        executable = hlint_toolchain.hlint,
        arguments = [args],
    )

    outputs = depset(
        direct = [output],
        transitive = [_collect_hlint_logs(ctx.rule.attr.deps)],
    )
    lint_info = HaskellHLintInfo(outputs = outputs)
    output_files = OutputGroupInfo(default = outputs)
    return [lint_info, output_files]

haskell_lint_aspect = aspect(
    _haskell_lint_aspect_impl,
    attr_aspects = ["deps"],
    toolchains = [
        "@io_tweag_rules_haskell//haskell/lint:toolchain",
    ],
)
"""Lint Haskell source files using hlint.

Applies [hlint][hlint] to the Haskell sources of a target and its transitive
dependencies. Reports linter warnings to stdout as well as to generated html
files. Fails if the linter produces any warnings.

Requires an `hlint_toolchain` to be registered.

Example:
  ```
  $ bazel test //some/haskell:target --aspects @io_tweag_rules_haskell//haskell:lint.bzl%haskell_lint_aspect
  ```

[hlint]: https://github.com/ndmitchell/hlint#readme
"""

def _haskell_lint_rule_impl(ctx):
    return [DefaultInfo(
        files = _collect_hlint_logs(ctx.attr.deps),
    )]

haskell_lint = rule(
    _haskell_lint_rule_impl,
    attrs = {
        "deps": attr.label_list(
            aspects = [haskell_lint_aspect],
            doc = "List of Haskell targets to lint.",
        ),
    },
)
"""Lint Haskell source files using hlint.

Applies [hlint][hlint] to the Haskell sources of the targets specified in
`deps` and their transitive dependencies. Reports linter warnings to stdout as
well as to generated html files. Acts as a test and fails if the linter
produces any warnings.

Requires an `hlint_toolchain` to be registered.

Example:
  ```
  haskell_library(
    name = "my-lib",
    ...
  )

  haskell_lint(
    name = "my-lib-hlint",
    deps = [":my-lib"],
  )
  ```

[hlint]: https://github.com/ndmitchell/hlint#readme
"""

def _hlint_toolchain_impl(ctx):
    return [platform_common.ToolchainInfo(
        name = ctx.label.name,
        hlint = ctx.executable.hlint,
        hint = ctx.file.hint,
    )]

_hlint_toolchain = rule(
    _hlint_toolchain_impl,
    attrs = {
        "hlint": attr.label(
            allow_single_file = True,
            cfg = "host",
            doc = "The hlint executable.",
            executable = True,
            mandatory = True,
        ),
        "hint": attr.label(
            allow_single_file = True,
            doc = "The hint/ignore file to use.",
        ),
    },
)

def hlint_toolchain(name, hlint, hint = None, **kwargs):
    """Define an hlint toolchain.

    Args:
      hlint: The hlint executable.
      hint: (optional) The .hlint.yaml hint file.

    """
    impl_name = name + "-impl"
    _hlint_toolchain(
        name = impl_name,
        hlint = hlint,
        visibility = ["//visibility:public"],
        **kwargs
    )
    native.toolchain(
        name = name,
        toolchain_type = "@io_tweag_rules_haskell//haskell/lint:toolchain",
        toolchain = ":" + impl_name,
    )
