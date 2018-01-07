# Can't be defined in haskell.bzl due to circular dependencies.
def haskell_repositories():
  native.http_archive(
    name = "bazel_skylib",
    strip_prefix = "bazel-skylib-0.2.0",
    urls = ["https://github.com/bazelbuild/bazel-skylib/archive/0.2.0.tar.gz"],
  )
