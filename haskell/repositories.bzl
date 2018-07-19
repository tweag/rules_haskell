"""Workspace rules (repositories)"""

def haskell_repositories():
    """Provide all repositories that are necessary for `rules_haskell` to
    function.
    """
    native.http_archive(
        name = "bazel_skylib",
        strip_prefix = "bazel-skylib-0.2.0",
        urls = ["https://github.com/bazelbuild/bazel-skylib/archive/0.2.0.tar.gz"],
    )
