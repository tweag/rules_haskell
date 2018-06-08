"""Derived context with Haskell-specific fields and methods"""

load("@bazel_skylib//:lib.bzl", "paths")

HaskellContext = provider()

def haskell_context(ctx, attr=None):
  toolchain = ctx.toolchains["@io_tweag_rules_haskell//haskell:toolchain"]

  if not attr:
    attr = ctx.attr

  src_root = paths.join(
    ctx.label.workspace_root,
    ctx.label.package,
    attr.src_strip_prefix,
  )

  env = {
    "PATH": toolchain.visible_bin_path
  }

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
