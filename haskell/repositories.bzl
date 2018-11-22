"""Workspace rules (repositories)"""

load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")

def haskell_repositories():
    """Provide all repositories that are necessary for `rules_haskell` to
    function.
    """
    git_repository(
        name = "bazel_skylib",
        remote = "https://github.com/bazelbuild/bazel-skylib.git",
        commit = "d7c5518fa061ae18a20d00b14082705d3d2d885d",  # 2018-11-21
    )
