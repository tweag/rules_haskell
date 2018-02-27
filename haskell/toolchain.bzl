"""Rules for defining toolchains"""

def _haskell_toolchain_impl(ctx):
  for tool in ["ghc", "ghc-pkg", "hsc2hs", "haddock", "ghci"]:
    if tool not in [t.basename for t in ctx.files.tools]:
      fail("Cannot find {} in {}".format(tool, ctx.attr.tools.label))

  # Run a version check on the compiler.
  for t in ctx.files.tools:
    if t.basename == "ghc":
      compiler = t
  version_file = ctx.actions.declare_file("ghc-version")
  arguments = ctx.actions.args()
  arguments.add(compiler)
  arguments.add(version_file)
  arguments.add(ctx.attr.version)
  ctx.actions.run(
    inputs = [compiler],
    outputs = [version_file],
    executable = ctx.file._ghc_version_check,
    arguments = [arguments],
  )

  return [
    platform_common.ToolchainInfo(
      name = ctx.label.name,
      tools = ctx.files.tools,
      version = ctx.attr.version,
    ),
    # Make everyone implicitly depend on the version_file, to force
    # the version check.
    DefaultInfo(files = depset([version_file])),
    ]

_haskell_toolchain = rule(
  _haskell_toolchain_impl,
  attrs = {
    "tools": attr.label(mandatory = True),
    "version": attr.string(mandatory = True),
    "_ghc_version_check": attr.label(
      allow_single_file = True,
      default = Label("@io_tweag_rules_haskell//haskell:ghc-version-check.sh")
    )
  }
)

def haskell_toolchain(name, version, tools, **kwargs):
  """Declare a compiler toolchain.

  Declares a compiler toolchain. You need at least one of these declared
  somewhere in your `BUILD` files for the other rules to work. Once
  declared, you then need to *register* the toolchain using
  `register_toolchain` in your `WORKSPACE` file (see Example below).

  Example:
    ```bzl
    haskell_toolchain(
        name = "ghc",
        version = '1.2.3'
        tools = ["@sys_ghc//:bin"]
    )
    ```

    where `@ghc` is an external repository defined in the `WORKSPACE`,
    e.g. using:

    ```bzl
    nixpkgs_package(
        name = 'sys_ghc',
        attribute_path = 'haskell.compiler.ghc123'
    )

    register_toolchain("//:ghc")
    ```
  """
  impl_name = name + "-impl"
  _haskell_toolchain(
    name = impl_name,
    version = version,
    tools = tools,
    visibility = ["//visibility:public"],
    **kwargs
  )
  native.toolchain(
    name = name,
    toolchain_type = "@io_tweag_rules_haskell//haskell:toolchain",
    toolchain = ":" + impl_name,
    exec_compatible_with = [
      '@bazel_tools//platforms:linux',
      '@bazel_tools//platforms:x86_64'],
    target_compatible_with = [
      '@bazel_tools//platforms:linux',
      '@bazel_tools//platforms:x86_64'],
  )
