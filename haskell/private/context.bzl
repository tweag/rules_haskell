"""Derived context with Haskell-specific fields and methods"""

load("@bazel_skylib//:lib.bzl", "paths")

HaskellContext = provider()

def haskell_context(ctx, attr = None):
    toolchain = ctx.toolchains["@io_tweag_rules_haskell//haskell:toolchain"]

    if not attr:
        attr = ctx.attr

    if hasattr(attr, "src_strip_prefix"):
        src_strip_prefix = attr.src_strip_prefix
    else:
        src_strip_prefix = ""

    src_root = paths.join(
        ctx.label.workspace_root,
        ctx.label.package,
        src_strip_prefix,
    )

    env = {
        "PATH": toolchain.visible_bin_path,
        "LANG": toolchain.locale,
    }

    if toolchain.locale_archive != None:
        env["LOCALE_ARCHIVE"] = toolchain.locale_archive.path

    return HaskellContext(
        # Fields
        name = attr.name,
        label = ctx.label,
        toolchain = toolchain,
        tools = toolchain.tools,
        tools_runfiles = toolchain.tools_runfiles,
        extra_binaries = toolchain.extra_binaries,
        src_root = src_root,
        env = env,
        mode = ctx.var["COMPILATION_MODE"],
        actions = ctx.actions,
        bin_dir = ctx.bin_dir,
        genfiles_dir = ctx.genfiles_dir,
    )
