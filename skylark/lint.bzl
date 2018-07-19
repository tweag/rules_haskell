"""Lint checks for BUILD and *.bzl files."""

# sh_binary(
#   name = "buildify",
#   srcs = ["apply.sh"],
#   data = ["@com_github_bazelbuild_buildtools//buildifier", ":srcs"],
#   args = [
#     "$(location @com_github_bazelbuild_buildtools//buildifier)",
#     "$(locations :srcs)",
#   ]
# )

def _buildifier_generate_impl(ctx):
    output = ctx.actions.declare_file(ctx.attr.name)
    ctx.actions.write(
        output = output,
        is_executable = True,
        content = """
    {buildifier} -mode=check {args}
    """.format(
            buildifier = ctx.file._buildifier.short_path,
            args = " ".join([entry.path for entry in ctx.files.data]),
        ),
    )
    return DefaultInfo(
        files = depset([output]),
        runfiles = ctx.runfiles(
            files = [ctx.file._buildifier] + ctx.files.data,
            collect_data = True,
        ),
    )

_buildifier_generate = rule(
    _buildifier_generate_impl,
    attrs = {
        "data": attr.label_list(allow_files = True, cfg = "data"),
        "_buildifier": attr.label(
            default = "@com_github_bazelbuild_buildtools//buildifier",
            single_file = True,
        ),
    },
)

def skylark_lint(name = "lint", data = None):
    """Add a lint test to check style of BUILD and *.bzl files."""
    if not data:
        data = native.glob(["**/*.bzl", "**/BUILD"])
    script_name = name + "_buildifier_generate"
    _buildifier_generate(
        name = script_name,
        data = data,
    )
    native.sh_test(
        name = name + "_buildifier",
        srcs = [script_name],
        data = data,
        tags = ["lint"],
    )
