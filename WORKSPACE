workspace(name = "ai_formation_hazel")

http_archive(
    name = "io_tweag_rules_nixpkgs",
    strip_prefix = "rules_nixpkgs-cd2ed701127ebf7f8f21d37feb1d678e4fdf85e5",
    urls = ["https://github.com/tweag/rules_nixpkgs/archive/cd2ed70.tar.gz"],
)

load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl", "nixpkgs_git_repository", "nixpkgs_package")

nixpkgs_git_repository(
    name = "nixpkgs",
    # A revision of 17.09 that contains ghc-8.2.2:
    revision = "c33c5239f62b4855b14dc5b01dfa3e2a885cf9ca",
)

RULES_HASKELL_SHA = "e1eba61f145b5203c748fc53e77e0862e2fc2554"
http_archive(
    name = "io_tweag_rules_haskell",
    urls = ["https://github.com/tweag/rules_haskell/archive/"
            + RULES_HASKELL_SHA + ".tar.gz"],
    strip_prefix = "rules_haskell-" + RULES_HASKELL_SHA,
)

load("@io_tweag_rules_haskell//haskell:repositories.bzl", "haskell_repositories")
haskell_repositories()

nixpkgs_package(
    name = "ghc",
    repository = "@nixpkgs",
    attribute_path = "haskell.packages.ghc822.ghc",
    build_file = "@ai_formation_hazel//:BUILD.ghc",
)

register_toolchains("@ghc//:ghc")

load("//:hazel.bzl", "hazel_repositories")
load("//:packages.bzl", "packages", "prebuilt_dependencies")

hazel_repositories(
    packages=packages,
    prebuilt_dependencies=prebuilt_dependencies)
