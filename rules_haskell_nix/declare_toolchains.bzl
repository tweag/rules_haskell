load(
    "@hub//:nix_ghcs.bzl",
    "ghc_labels",
    "toolchain_keys",
    "toolchains_2",
)
load("@rules_haskell_nix//:nixpkgs.bzl", "register_ghc_from_nixpkgs_package")

def _nix_toolchains_repo_impl(rctx):
    content = "\n".join(rctx.attr.toolchain_declarations)
    rctx.file("BUILD.bazel", content = content)

_nix_toolchains_repo = repository_rule(
    implementation = _nix_toolchains_repo_impl,
    attrs = {
        "toolchain_declarations": attr.string_list(),
    },
)

def _declare_nix_toolchains_impl(mctx):
    # Instead of creating one external repository for each `toolchain(...)` declaration,
    # we recover them in a list and declare them all in the `all_bindist_toolchains` repository.
    # Following bazel's iteration order over modules so that toolchains declared by the root module take precedence

    # an alternative would be to use aliases to the (to get rid of the toolchain_declarations parameter)
    # if/once the following issue is resolved
    # https://github.com/bazelbuild/bazel/issues/16298
    toolchain_declarations = []
    for toolchain in toolchain_keys:
        kwargs = dict(toolchains_2[toolchain])
        kwargs["register"] = False
        kwargs["toolchain_declarations"] = toolchain_declarations
        kwargs["nixpkgs_ghc"] = ghc_labels[toolchain]
        kwargs["module_ctx"] = mctx
        register_ghc_from_nixpkgs_package(**kwargs)
    _nix_toolchains_repo(
        name = "all_nix_toolchains",
        toolchain_declarations = toolchain_declarations,
    )

declare_nix_toolchains = module_extension(
    implementation = _declare_nix_toolchains_impl,
)
