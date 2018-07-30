"""Hoogle support"""

load(
    ":private/providers.bzl",
    "HaskellBinaryInfo",
    "HaskellBuildInfo",
    "HaskellLibraryInfo",
)
load(":private/context.bzl", "haskell_context")
load(":private/set.bzl", "set")
load(
    ":private/path_utils.bzl",
    "get_external_libs_path",
    "get_lib_name",
    "target_unique_name",
)
load("@bazel_skylib//:lib.bzl", "dicts", "paths")

def _hoogle_toolchain_impl(ctx):
    return platform_common.ToolchainInfo(
        name = ctx.label.name,
        hoogle = ctx.files.hoogle,
    )

_hoogle_toolchain = rule(
    _hoogle_toolchain_impl,
    attrs = {
        "hoogle": attr.label(
            doc = "Hoogle executable",
            cfg = "host",
            executable = True,
            single_file = True,
            mandatory = True,
        ),
    },
)

def haskell_hoogle_toolchain(name, hoogle, **kwargs):
    impl_name = name + "-impl"
    _hoogle_toolchain(
        name = impl_name,
        hoogle = hoogle,
        visibility = ["//visibility:public"],
        **kwargs
    )
    native.toolchain(
        name = name,
        toolchain_type = "@io_tweag_rules_haskell//haskell:hoogle-toolchain",
        toolchain = ":" + impl_name,
    )
