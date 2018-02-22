"""Workspace rules (repositories)"""

# Re-export binutils_repository
# load(":binutils-repo.bzl",
#   _binutils_repository = "binutils_repository",
# )

def haskell_repositories():
  """Provide all repositories that are necessary for `rules_haskell` to
  function.
  """
  native.http_archive(
    name = "bazel_skylib",
    strip_prefix = "bazel-skylib-0.2.0",
    urls = ["https://github.com/bazelbuild/bazel-skylib/archive/0.2.0.tar.gz"],
  )

# binutils_repository = _binutils_repository
