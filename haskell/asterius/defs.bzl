load("@bazel_skylib//lib:paths.bzl", "paths")

def _asterius_toolchain_impl(ctx):
    ahc_dist = None
    for file in ctx.files.binaries:
        basename_no_ext = paths.split_extension(file.basename)[0]
        if basename_no_ext == "ahc-dist":
            ahc_dist = file
    if ahc_dist == None:
        fail("ahc-dist was not found when defining the asterius toolchain")

    return [
        platform_common.ToolchainInfo(
            name = ctx.label.name,
            ahc_dist = ahc_dist,
            tools = ctx.files.tools,
        ),
    ]

asterius_toolchain = rule(
    _asterius_toolchain_impl,
    attrs = {
        "binaries": attr.label_list(
            mandatory = True,
            doc = "The asterius top level wrappers",
        ),
        "tools": attr.label_list(
            mandatory = True,
            doc = "The complete asterius bundle, which is needed to execute the wrappers.",
        ),
    },
    doc = "Toolchain for asterius tools that are not part of the regular haskell toolchain",
)
