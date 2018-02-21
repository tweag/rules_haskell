package(default_visibility = ["//visibility:public"])

def _binutils_toolchain_impl(ctx):
  return [
    platform_common.ToolchainInfo(
      name = ctx.label.name,
      tools = ctx.attr.tools,
    )
  ]

rule(
  _binutils_toolchain_impl,
  attrs = {
    "tools": attrs.string_list(mandatory = True),
  }
)(
  name = "binutils_toolchain",
  tools = $(tools),
)

toolchain(
  name = "binutils",
  toolchain_type = "@io_tweag_rules_haskell//haskell:binutils-toolchain",
  toolchain = ":binutils_toolchain",
  exec_compatible_with = [
    '@bazel_tools//platforms:linux',
    '@bazel_tools//platforms:x86_64'],
  target_compatible_with = [
    '@bazel_tools//platforms:linux',
    '@bazel_tools//platforms:x86_64'],
)
