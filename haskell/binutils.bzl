"""TODO"""

def _binutils_toolchain_impl(ctx):
  return [
    platform_common.ToolchainInfo(
      name = ctx.label.name,
      tools = ctx.attr.tools,
    )
  ]

_binutils_toolchain = rule(
  _binutils_toolchain_impl,
  attrs = {
    "tools": attr.string_list(mandatory = True),
  }
)

def binutils_toolchain(name):
  """TODO
  """
  impl_name = name + "-impl"
  _binutils_toolchain(
    name = impl_name,
    tools = [
      "ln_location",
      "grep_location",
    ],
    visibility = ["//visibility:public"],
  )
  native.toolchain(
    name = name,
    toolchain_type = "@io_tweag_rules_haskell//haskell:binutils-toolchain",
    toolchain = ":" + impl_name,
    exec_compatible_with = [
      '@bazel_tools//platforms:linux',
      '@bazel_tools//platforms:x86_64'],
    target_compatible_with = [
      '@bazel_tools//platforms:linux',
      '@bazel_tools//platforms:x86_64'],
  )
