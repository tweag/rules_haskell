def _ghc_nixpkgs_toolchain_impl(repository_ctx):
    repository_ctx.file(
        "BUILD",
        executable = False,
        content = """
load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_import",
    "haskell_toolchain",
)

haskell_import(
    name = "rts",
    shared_library = 'libfoo.so',
    static_library = 'libfoo.a',
    visibility = ["//visibility:public"],
)

haskell_toolchain(
    name = "toolchain",
    libraries = [":rts"],
)
        """,
    )

_ghc_nixpkgs_toolchain = repository_rule(
    _ghc_nixpkgs_toolchain_impl,
)

def haskell_register_ghc_nixpkgs():
    _ghc_nixpkgs_toolchain(
        name = "io_tweag_rules_haskell_ghc-nixpkgs-toolchain",
    )
    native.register_toolchains("@io_tweag_rules_haskell_ghc-nixpkgs-toolchain//:toolchain")
