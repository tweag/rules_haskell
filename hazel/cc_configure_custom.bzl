load("@bazel_tools//tools/cpp:cc_configure.bzl", "cc_autoconf_impl")

def _cc_configure_custom(ctx):
    overriden_tools = {
        "gcc": ctx.path(ctx.attr.gcc),
        "ld": ctx.path(ctx.attr.ld),
    }
    return cc_autoconf_impl(ctx, overriden_tools)

cc_configure_custom = repository_rule(
    implementation = _cc_configure_custom,
    attrs = {
        "gcc": attr.label(
            executable = True,
            cfg = "host",
            allow_single_file = True,
            doc = "`gcc` to use in cc toolchain",
        ),
        "ld": attr.label(
            executable = True,
            cfg = "host",
            allow_single_file = True,
            doc = "`ld` to use in cc toolchain",
        ),
    },
    local = True,
)
"""Overwrite cc toolchain by supplying custom `gcc` and `ld` (e.g. from
Nix). This allows to fix mismatch of `gcc` versions between what is used by
packages that come from Nix (e.g. `ghc`) and what Bazel detects
automatically (i.e. system-level `gcc`).
"""
