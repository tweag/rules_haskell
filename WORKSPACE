workspace(name = "io_tweag_rules_haskell")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@io_tweag_rules_haskell//haskell:repositories.bzl", "haskell_repositories")

# bazel dependencies
haskell_repositories()

rules_nixpkgs_version = "0.5.2"

rules_nixpkgs_version_is_hash = False

rules_nixpkgs_sha256 = "5a384daa57b49abf9f0b672852f1a66a3c52aecf9d4d2ac64f6de0fd307690c8"

http_archive(
    name = "io_tweag_rules_nixpkgs",
    sha256 = rules_nixpkgs_sha256,
    strip_prefix = "rules_nixpkgs-%s" % rules_nixpkgs_version,
    urls = ["https://github.com/tweag/rules_nixpkgs/archive/%s.tar.gz" % rules_nixpkgs_version] if rules_nixpkgs_version_is_hash else ["https://github.com/tweag/rules_nixpkgs/archive/v%s.tar.gz" % rules_nixpkgs_version],
)

load(
    "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_cc_configure",
    "nixpkgs_local_repository",
)

nixpkgs_local_repository(
    name = "nixpkgs",
    nix_file = "//nixpkgs:default.nix",
)

load(
    "@io_tweag_rules_haskell//haskell:nixpkgs.bzl",
    "haskell_register_ghc_nixpkgs",
)

haskell_register_ghc_nixpkgs(
    locale_archive = "@glibc_locales//:locale-archive",
    nix_file = "//tests:ghc.nix",
    nix_file_deps = ["//nixpkgs:default.nix"],
    version = "8.6.4",
)

nixpkgs_cc_configure(
    nix_file = "//nixpkgs:cc-toolchain.nix",
    repository = "@nixpkgs",
)
