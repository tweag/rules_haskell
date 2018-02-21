"""Workspace rules (repositories)"""

load(":binutils-toolchain.bzl",
     "binutils_toolchain_repository")

def haskell_repositories():
  """Provide all repositories that are necessary for `rules_haskell` to
  function.
  """
  native.http_archive(
    name = "bazel_skylib",
    strip_prefix = "bazel-skylib-0.2.0",
    urls = ["https://github.com/bazelbuild/bazel-skylib/archive/0.2.0.tar.gz"],
  )
  binutils_toolchain_repository(
    name = "io_tweag_rules_haskell_binutils",
    provide_tools = ["ln", "grep"],
  )
  native.register_toolchains("@io_tweag_rules_haskell_binutils//:binutils")
