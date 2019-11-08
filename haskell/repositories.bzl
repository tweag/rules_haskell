"""Workspace rules (repositories)"""

load(":ghc_bindist.bzl", "haskell_register_ghc_bindists")
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

def rules_haskell_toolchains(**kwargs):
    """Register GHC binary distributions for all platforms as toolchains."""
    haskell_register_ghc_bindists(**kwargs)

def rules_haskell_dependencies():
    """Provide all repositories that are necessary for `rules_haskell` to
    function.
    """
    excludes = native.existing_rules().keys()

    if "bazel_skylib" not in excludes:
        http_archive(
            name = "bazel_skylib",
            sha256 = "eb5c57e4c12e68c0c20bc774bfbc60a568e800d025557bc4ea022c6479acc867",
            strip_prefix = "bazel-skylib-0.6.0",
            urls = ["https://github.com/bazelbuild/bazel-skylib/archive/0.6.0.tar.gz"],
        )

    if "io_tweag_rules_nixpkgs" not in excludes:
        http_archive(
            name = "io_tweag_rules_nixpkgs",
            sha256 = "b13cb651e972b3aa06bab75603923071d7647c9ee3f8acb707e3831aa606e911",
            strip_prefix = "rules_nixpkgs-bdc802f8e716c66d50cc91f7ad22505edf13c48b",
            urls = ["https://github.com/tweag/rules_nixpkgs/archive/bdc802f8e716c66d50cc91f7ad22505edf13c48b.tar.gz"],
        )

def haskell_repositories():
    """DEPRECATED alias for rules_haskell_dependencies"""
    rules_haskell_dependencies()
