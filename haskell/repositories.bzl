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
            sha256 = "5a384daa57b49abf9f0b672852f1a66a3c52aecf9d4d2ac64f6de0fd307690c8",
            strip_prefix = "rules_nixpkgs-0.5.2",
            urls = ["https://github.com/tweag/rules_nixpkgs/archive/v0.5.2.tar.gz"],
        )

def haskell_repositories():
    """DEPRECATED alias for rules_haskell_dependencies"""
    rules_haskell_dependencies()
