"""Derived context with Haskell-specific fields and methods"""

load("@bazel_skylib//lib:paths.bzl", "paths")

HaskellContext = provider()

def haskell_context(ctx):
    toolchain = ctx.toolchains["@io_tweag_rules_haskell//haskell:toolchain"]

    attr = ctx.attr

    return HaskellContext(
        # Fields
        name = attr.name,
        label = ctx.label,
        toolchain = toolchain,
    )
