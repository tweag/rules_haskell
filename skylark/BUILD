exports_files(["lint.bzl"])

sh_binary(
    name = "buildifier",
    srcs = ["buildifier.sh"],
    args = ["$(location @com_github_bazelbuild_buildtools//buildifier)"],
    data = ["@com_github_bazelbuild_buildtools//buildifier"],
)
