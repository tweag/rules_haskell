"""Lint Haskell sources using hlint."""

load("@bazel_skylib//lib:dicts.bzl", "dicts")
load("@bazel_skylib//lib:paths.bzl", "paths")
load(
    "@io_tweag_rules_haskell//haskell:private/path_utils.bzl",
    "match_label",
    "parse_pattern",
)
load("@io_tweag_rules_haskell//haskell:providers.bzl", "HaskellInfo")

HaskellHLintInfo = provider(
    doc = "Provider that collects files produced by hlint",
    fields = {
        "outputs": "dict from target Label to hlint results.",
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

    html_out = ctx.actions.declare_file(target.label.name + "-hlint.html")
    txt_out = ctx.actions.declare_file(target.label.name + "-hlint.txt")
    status_out = ctx.actions.declare_file(target.label.name + "-hlint.status")
    args = ctx.actions.args()
    if hint:
        args.add(hint, format = "--hint=%s")
    args.add_all(inputFiles)
    args.add(html_out, format = "--report=%s")
    args.add("--verbose")

    ctx.actions.run_shell(
        command = """
        {hlint} $@ >{txt} && echo 0 >{status} || echo 1 >{status}
        """.format(
            hlint = hlint_toolchain.hlint.path,
            txt = txt_out.path,
            status = status_out.path,
        ),
        inputs = ([hint] if hint else []) + inputFiles,
        outputs = [html_out, txt_out, status_out],
        mnemonic = "HaskellHLint",
        progress_message = "HaskellHLint {}".format(ctx.label),
        tools = [hlint_toolchain.hlint],
        arguments = [args],
    )

    script_out = ctx.actions.declare_file(target.label.name + "-hlint.sh")
    ctx.actions.write(
        output = script_out,
        is_executable = True,
        content = """#!/usr/bin/env bash
read status <$(rlocation {status})
if [ $status -ne 0 ]; then
    cat $(rlocation {txt})
    return $status
fi
""".format(
            txt = paths.join(ctx.workspace_name, txt_out.short_path),
            status = paths.join(ctx.workspace_name, status_out.short_path),
        ),
    )

    outputs = _collect_hlint_logs(ctx.rule.attr.deps)
    outputs[target.label] = struct(
        report = html_out,
        runfiles = depset([txt_out, status_out, script_out]),
        script = script_out,
    )
    lint_info = HaskellHLintInfo(outputs = outputs)
    output_files = OutputGroupInfo(default = [html_out, txt_out])
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

_bash_runfiles_boilerplate = """\
# Copy-pasted from Bazel's Bash runfiles library (tools/bash/runfiles/runfiles.bash).
set -euo pipefail
if [[ ! -d "${RUNFILES_DIR:-/dev/null}" && ! -f "${RUNFILES_MANIFEST_FILE:-/dev/null}" ]]; then
  if [[ -f "$0.runfiles_manifest" ]]; then
    export RUNFILES_MANIFEST_FILE="$0.runfiles_manifest"
  elif [[ -f "$0.runfiles/MANIFEST" ]]; then
    export RUNFILES_MANIFEST_FILE="$0.runfiles/MANIFEST"
  elif [[ -f "$0.runfiles/bazel_tools/tools/bash/runfiles/runfiles.bash" ]]; then
    export RUNFILES_DIR="$0.runfiles"
  fi
fi
if [[ -f "${RUNFILES_DIR:-/dev/null}/bazel_tools/tools/bash/runfiles/runfiles.bash" ]]; then
  source "${RUNFILES_DIR}/bazel_tools/tools/bash/runfiles/runfiles.bash"
elif [[ -f "${RUNFILES_MANIFEST_FILE:-/dev/null}" ]]; then
  source "$(grep -m1 "^bazel_tools/tools/bash/runfiles/runfiles.bash " \
            "$RUNFILES_MANIFEST_FILE" | cut -d ' ' -f 2-)"
else
  echo >&2 "ERROR: cannot find @bazel_tools//tools/bash/runfiles:runfiles.bash"
  exit 1
fi
# --- end runfiles.bash initialization ---
"""

def _haskell_lint_rule_impl(ctx):
    whitelist = [parse_pattern(ctx, pat) for pat in ctx.attr.experimental_lint_whitelist]
    blacklist = [parse_pattern(ctx, pat) for pat in ctx.attr.experimental_lint_blacklist]

    results = [
        result
        for (label, result) in _collect_hlint_logs(ctx.attr.deps).items()
        if _should_lint_target(whitelist, blacklist, label)
    ]

    script_out = ctx.actions.declare_file(ctx.label.name + "-hlint.sh")
    ctx.actions.write(
        output = script_out,
        is_executable = True,
        content = """#!/usr/bin/env bash
{bash_runfiles_boilerplate}
final_status=0
{results}
exit $final_status
""".format(
            bash_runfiles_boilerplate = _bash_runfiles_boilerplate,
            results = "\n".join([
                "source $(rlocation {script}) || final_status=1".format(script = paths.join(ctx.workspace_name, result.script.short_path))
                for result in results
            ]),
        ),
    )

    return [DefaultInfo(
        executable = script_out,
        files = depset([result.report for result in results]),
        runfiles = ctx.runfiles(
            files = ctx.files._bash_runfiles,
            transitive_files = depset(
                transitive = [result.runfiles for result in results],
            ),
        ),
    )]

haskell_lint_test = rule(
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
        "_bash_runfiles": attr.label(default = Label("@bazel_tools//tools/bash/runfiles")),
    },
    test = True,
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

  haskell_lint_test(
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
