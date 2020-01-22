load(":private/context.bzl", "haskell_context")
load(":private/actions/info.bzl", "write_proto_file")

def _haskell_toolchain_info_impl(ctx):
    hs = haskell_context(ctx)
    pb = write_proto_file(
        hs,
        ctx.label.name,
        "haskell.GhcConfig",
        struct(ghc = hs.tools.ghc.path),
    )

    return [DefaultInfo(files = depset([pb]))]

haskell_toolchain_info = rule(
    implementation = _haskell_toolchain_info_impl,
    toolchains = ["@rules_haskell//haskell:toolchain"],
)
