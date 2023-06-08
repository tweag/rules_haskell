""" This module extension contains non module dependencies which are only used with bzlmod """

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load(
    "@rules_haskell_nix//:nixpkgs.bzl",
    "haskell_register_ghc_nixpkgs",
)
load(
    "@rules_haskell_tests//:non_module_deps_1.bzl",
    "test_cabalopts",
    "test_ghcopts",
    "test_haddock_flags",
    "test_repl_ghci_args",
)
load(
    "@rules_haskell//:constants.bzl",
    "test_ghc_version",
)
load("@rules_nixpkgs_nodejs//:nodejs.bzl", "nixpkgs_nodejs_configure_platforms")

def _non_module_deps_bzlmod_impl(_ctx):
    # Only used with bzlmod, because in WORKSPACE bzlmod we test the
    # backward compatible "@rules_haskell//haskell:nixpkgs.bzl"
    haskell_register_ghc_nixpkgs(
        attribute_path = "",
        cabalopts = test_cabalopts,
        ghcopts = test_ghcopts,
        haddock_flags = test_haddock_flags,
        locale_archive = "@glibc_locales//:locale-archive",
        nix_file_content = """with import <nixpkgs> {}; haskell.packages.ghc925.ghc""",
        repl_ghci_args = test_repl_ghci_args,
        repository = "@nixpkgs_default",
        version = test_ghc_version,
        register = False,
    )

    # Workspace mode uses the asterius_dependencies_nix convenience
    # macro which sets up the node toolchain and npm.  In bzlmod we
    # make use of rules_js module extension for npm and rely on
    # rules_nixpkgs_nodejs for the node toolchain.
    nixpkgs_nodejs_configure_platforms(
        name = "nixpkgs_nodejs",
        repository = "@nixpkgs_default",
        register = False,
    )

non_module_deps_bzlmod = module_extension(
    implementation = _non_module_deps_bzlmod_impl,
)
