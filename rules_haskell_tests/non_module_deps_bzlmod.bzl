""" This module extension contains non module dependencies which are only used with bzlmod """

load("@rules_nixpkgs_nodejs//:nodejs.bzl", "nixpkgs_nodejs_configure_platforms")

def _non_module_deps_bzlmod_impl(_ctx):
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
