"""Lint Haskell sources using hlint."""

load("@bazel_skylib//lib:dicts.bzl", "dicts")
load(
    "@io_tweag_rules_haskell//haskell:private/path_utils.bzl",
    "match_label",
    "parse_pattern",
)
load("@io_tweag_rules_haskell//haskell:providers.bzl", "HaskellInfo")

HaskellHLintInfo = provider(
    doc = "Provider that collects files produced by hlint",
    fields = {
        "outputs": "dict from target Label to hlint log File.",
    },
)

def _collect_hlint_logs(deps):
    return dicts.add(*[
        dep[HaskellHLintInfo].outputs
        for dep in deps
        if HaskellHLintInfo in dep
    ])

def _haskell_lint_aspect_impl(target, ctx):
    if not HaskellInfo in target:
        return []

    hlint_toolchain = ctx.toolchains["@io_tweag_rules_haskell//haskell/lint:toolchain"]
    hint = hlint_toolchain.hint

    inputFiles = [
        f
        for src in getattr(ctx.rule.attr, "srcs", [])
        for f in src.files
        if f.is_source and f.extension in ["chs", "hs", "hs-boot", "hsc", "lhs", "lhs-boot"]
    ]
    if len(inputFiles) == 0:
        return []

    output = ctx.actions.declare_file(target.label.name + "-hlint.html")
    args = ctx.actions.args()
    if hint:
        args.add(hint, format = "--hint=%s")
    args.add_all(inputFiles)
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

    outputs = _collect_hlint_logs(ctx.rule.attr.deps)
    outputs[target.label] = output
    lint_info = HaskellHLintInfo(outputs = outputs)
    output_files = OutputGroupInfo(default = [output])
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

def _should_lint_target(whitelist, blacklist, label):
    for pattern in blacklist:
        if match_label(pattern, label):
            return False

    for pattern in whitelist:
        if match_label(pattern, label):
            return True

    return False

def _haskell_lint_rule_impl(ctx):
    whitelist = [parse_pattern(ctx, pat) for pat in ctx.attr.experimental_lint_whitelist]
    blacklist = [parse_pattern(ctx, pat) for pat in ctx.attr.experimental_lint_blacklist]
    outputs = depset([
        log
        for (label, log) in _collect_hlint_logs(ctx.attr.deps).items()
        if _should_lint_target(whitelist, blacklist, label)
    ])

    return [DefaultInfo(files = outputs)]

haskell_lint = rule(
    _haskell_lint_rule_impl,
    attrs = {
        "deps": attr.label_list(
            aspects = [haskell_lint_aspect],
            doc = "List of Haskell targets to lint.",
        ),
        "experimental_lint_whitelist": attr.string_list(
            doc = """List of targets to lint.

            Wild-card targets such as //... or //:all are allowed.

            The black-list takes precedence over the white-list.

            Note, this attribute will change depending on the outcome of
            https://github.com/bazelbuild/bazel/issues/7763.
            """,
            default = ["//..."],
        ),
        "experimental_lint_blacklist": attr.string_list(
            doc = """List of targets to not lint.

            Wild-card targets such as //... or //:all are allowed.

            The black-list takes precedence over the white-list.

            Note, this attribute will change depending on the outcome of
            https://github.com/bazelbuild/bazel/issues/7763.
            """,
            default = [],
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
