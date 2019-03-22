"""Workspace rules (repositories)"""

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

def hazel_repositories():
    """Provide all repositories that are necessary for `hazel` to function.
    """
    excludes = native.existing_rules().keys()
    if "io_tweag_rules_nixpkgs" not in excludes:
        http_archive(
            name = "io_tweag_rules_nixpkgs",
            strip_prefix = "rules_nixpkgs-cd2ed701127ebf7f8f21d37feb1d678e4fdf85e5",
            urls = ["https://github.com/tweag/rules_nixpkgs/archive/cd2ed70.tar.gz"],
        )
    if "io_tweag_rules_haskell" not in excludes:
        http_archive(
            name = "io_tweag_rules_haskell",
            strip_prefix = "rules_haskell-869d14b8c7c95745b146be9ec15285d8c6dabe57",
            urls = ["https://github.com/FormationAI/rules_haskell/archive/869d14b.tar.gz"],
        )
