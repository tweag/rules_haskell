load(
    "@hub//:nix_ghcs.bzl",
    "ghc_labels",
    "toolchain_configs",
    "toolchain_keys",
    "toolchains_2",
)
load("@rules_haskell_nix//:nixpkgs.bzl", "register_ghc_from_nixpkgs_package")

def _nix_toolchains_repo_impl(rctx):
    content = "\n".join(rctx.attr.toolchain_declarations)
    print("content=", content)
    rctx.file("BUILD.bazel", content = content)

_nix_toolchains_repo = repository_rule(
    implementation = _nix_toolchains_repo_impl,
    attrs = {
        "toolchain_declarations": attr.string_list(),
    },
)

def _declare_nix_toolchains_impl(mctx):
    print("os from extension=", mctx.os.name)
    toolchain_declarations = []
    for toolchain in toolchain_keys:
        kwargs = dict(toolchains_2[toolchain])
        kwargs["register"] = False
        kwargs["toolchain_declarations"] = toolchain_declarations
        kwargs["nixpkgs_ghc"] = ghc_labels[toolchain]
        kwargs["module_ctx"] = mctx
        register_ghc_from_nixpkgs_package(**kwargs)
    print("toolchain_declarations=", toolchain_declarations)
    _nix_toolchains_repo(
        name = "all_nix_toolchains",
        toolchain_declarations = toolchain_declarations,
    )

declare_nix_toolchains = module_extension(
    implementation = _declare_nix_toolchains_impl,
)
