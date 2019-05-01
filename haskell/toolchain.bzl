"""Rules for defining toolchains"""

def _haskell_toolchain_impl(ctx):
    libraries = {
        lib.label.name: lib
        for lib in ctx.attr.libraries
    }

    return [
        platform_common.ToolchainInfo(
            name = ctx.label.name,
            libraries = libraries,
        ),
    ]

_haskell_toolchain = rule(
    _haskell_toolchain_impl,
    attrs = {
        "libraries": attr.label_list(
            doc = "The set of libraries that come with GHC.",
            mandatory = True,
        ),
    },
)

def haskell_toolchain(name, libraries):
    impl_name = name + "-impl"
    _haskell_toolchain(
        name = impl_name,
        libraries = libraries,
        visibility = ["//visibility:public"],
    )
    native.toolchain(
        name = name,
        toolchain_type = "@io_tweag_rules_haskell//haskell:toolchain",
        toolchain = ":" + impl_name,
    )
