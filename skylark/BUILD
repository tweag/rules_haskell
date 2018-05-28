exports_files(["lint.bzl"])

sh_binary(
  name = "buildifier",
  srcs = ["buildifier.sh"],
  data = ["@com_github_bazelbuild_buildtools//buildifier"],
  args = ["$(location @com_github_bazelbuild_buildtools//buildifier)"],
)
