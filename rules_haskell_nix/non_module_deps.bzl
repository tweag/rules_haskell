load("@rules_haskell//tools:os_info.bzl", "os_info")
load(
    "@rules_nixpkgs_core//:nixpkgs.bzl",
    "nixpkgs_local_repository",
)

def repositories(*, bzlmod):
    os_info(name = "os_info")

    nixpkgs_local_repository(
        name = "nixpkgs_default",
        nix_file = "@rules_haskell//nixpkgs:default.nix",
    )

def _non_module_deps_impl(_ctx):
    repositories(bzlmod = True)

non_module_deps = module_extension(
    implementation = _non_module_deps_impl,
)
