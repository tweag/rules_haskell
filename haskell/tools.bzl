"""Tools used during build."""

load(":set.bzl", "set")

def get_build_tools_path(ctx):
  """Get list of build tools suited for PATH.

  Useful to make sure that GHC can find e.g. hsc2hs at runtime: even
  if those files aren't expected, user may just be using OPTIONS_GHC
  to invoke them so they should be available.

  Args:
    ctx: Rule context.

  Returns:
    string: colon-separated paths to all build tools.
  """
  return ctx.toolchains["@io_tweag_rules_haskell//haskell:toolchain"].visible_bin_path

def get_ghc_version(ctx):
  """Get the GHC version.

  Args:
    ctx: Rule context.

  Returns:
    String: Version string.
  """
  return ctx.toolchains["@io_tweag_rules_haskell//haskell:toolchain"].version

def tools(ctx):
  """Return a structure containing all available tools.

  Args:
    ctx: Rule context.

  Returns:
    struct with Files inside.
  """
  return ctx.toolchains["@io_tweag_rules_haskell//haskell:toolchain"].tools
