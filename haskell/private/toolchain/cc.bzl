"""Definitions for customizing CC toolchains"""

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
            doc = "The `gcc` binary to use in the custom CC toolchain.",
            executable = True,
            cfg = "host",
            allow_single_file = True,
        ),
        "ld": attr.label(
            doc = "The `ld` binary to use in the custom CC toolchain.",
            executable = True,
            cfg = "host",
            allow_single_file = True,
        ),
    },
    local = True,
)
