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

def tools_runfiles(ctx):
  """Return a structure containing the runfiles for all available tools.

  Args:
    ctx: Rule context.

  Returns:
    struct with fields of lists of Files.
  """
  return ctx.toolchains["@io_tweag_rules_haskell//haskell:toolchain"].tools_runfiles

def is_darwin(ctx):
  """Returns whether the current build is using macOS.

  Args:
    ctx: Rule context.

  Returns:
    Boolean which is true when we're building on macOS.
  """
  return ctx.toolchains["@io_tweag_rules_haskell//haskell:toolchain"].is_darwin

def so_extension(ctx):
  """Returns the extension for shared libraries.

  Args:
    ctx: Rule context.

  Returns:
    string of extension.
  """
  return "dylib" if is_darwin(ctx) else "so"

def protobuf_tools(ctx):
  """Similarly to `tools`, return a structure containing all protobuf-related
  tools such as `protoc` or `protoc_gen_haskell`.

  Requires `@io_tweag_rules_haskell//protobuf:toolchain` toolchain to be
  registered.

  Args:
    ctx: Rule context.

  Returns:
    struct with Files inside.
  """
  return ctx.toolchains["@io_tweag_rules_haskell//protobuf:toolchain"].tools
