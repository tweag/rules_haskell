"""Rules for defining toolchains"""

_GHC_BINARIES = ["ghc", "ghc-pkg", "hsc2hs", "haddock", "ghci"]

load("@bazel_skylib//:lib.bzl",
     "paths",
)

load(":set.bzl",
     "set",
)

def _haskell_toolchain_impl(ctx):

  # Check that we have all that we want.
  for tool in _GHC_BINARIES:
    if tool not in [t.basename for t in ctx.files.tools]:
      fail("Cannot find {} in {}".format(tool, ctx.attr.tools.label))

  # Store the binaries of interest in ghc_binaries.
  ghc_binaries = []
  for tool in ctx.files.tools:
    if tool.basename in _GHC_BINARIES:
      ghc_binaries.append(tool.path)

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

  # NOTE The only way to let various executables know where other
  # executables are located is often only via the PATH environment variable.
  # For example, ghc uses gcc which is found on PATH. This forces us provide
  # correct PATH value (with e.g. gcc on it) when we call executables such
  # as ghc. However, this hurts hermeticity because if we construct PATH
  # naively, i.e. through concatenation of directory names of all
  # executables of interest, we also make other neighboring executables
  # visible, and they indeed can influence the building process, which is
  # undesirable.
  #
  # To prevent this, we create a special directory populated with symbolic
  # links to all executables we want to make visible, and only to them. Then
  # we make all rules depend on some of these symlinks, and provide PATH
  # (always the same) that points only to this prepared directory with
  # symlinks. This helps hermeticity and forces us to be explicit about all
  # programs we need/use for building. Setting PATH and listing as inputs
  # are both necessary for a tool to be available with this approach.

  visible_binaries = "visible-binaries"
  symlinks = set.empty()

  targets_r = [
    # CPP host fragments.
    ctx.host_fragments.cpp.ar_executable,
    ctx.host_fragments.cpp.compiler_executable,
    ctx.host_fragments.cpp.ld_executable,
    ctx.host_fragments.cpp.nm_executable,
    ctx.host_fragments.cpp.preprocessor_executable,
    ctx.host_fragments.cpp.strip_executable,
  ] + ghc_binaries # Previously collected GHC binaries.

  for target in targets_r:
    symlink = ctx.actions.declare_file(
      paths.join(visible_binaries, paths.basename(target))
    )
    ctx.actions.run(
      inputs = ctx.files.tools,
      outputs = [symlink],
      executable = ctx.file._make_bin_symlink_realpath,
      # FIXME Currently this part of the process is not hermetic. This
      # should be adjusted when
      #
      # https://github.com/bazelbuild/bazel/issues/4681
      #
      # is implemented.
      use_default_shell_env = True,
      arguments = [
        target,
        symlink.path,
      ],
    )
    set.mutable_insert(symlinks, symlink)

  targets_w = [
    "ln",
    "grep",
    "tee",
  ]

  for target in targets_w:
    symlink = ctx.actions.declare_file(
      paths.join(visible_binaries, paths.basename(target))
    )
    ctx.actions.run(
      inputs = ctx.files.tools,
      outputs = [symlink],
      executable = ctx.file._make_bin_symlink_which,
      use_default_shell_env = True, # FIXME see above
      arguments = [
        target,
        symlink.path,
      ],
    )
    set.mutable_insert(symlinks, symlink)

  tools_struct_args = {tool.basename.replace("-", "_"): tool
                       for tool in set.to_list(symlinks)}

  return [
    platform_common.ToolchainInfo(
      name = ctx.label.name,
      tools = struct(**tools_struct_args),
      # All symlinks are guaranteed to be in the same directory so we just
      # provide directory name of the first one (the collection cannot be
      # empty). The rest of the program may rely consider visible_bin_path
      # as the path to visible binaries, without recalculations.
      visible_bin_path = set.to_list(symlinks)[0].dirname,
      version = ctx.attr.version,
    ),
    # Make everyone implicitly depend on the version_file, to force
    # the version check.
    DefaultInfo(files = depset([version_file])),
    ]

_haskell_toolchain = rule(
  _haskell_toolchain_impl,
  host_fragments = ["cpp"],
  attrs = {
    "tools": attr.label(mandatory = True),
    "version": attr.string(mandatory = True),
    "_ghc_version_check": attr.label(
      allow_single_file = True,
      default = Label("@io_tweag_rules_haskell//haskell:ghc-version-check.sh")
    ),
    "_make_bin_symlink_realpath": attr.label(
      allow_single_file = True,
      default = Label("@io_tweag_rules_haskell//haskell:make-bin-symlink-realpath.sh")
    ),
    "_make_bin_symlink_which": attr.label(
      allow_single_file = True,
      default = Label("@io_tweag_rules_haskell//haskell:make-bin-symlink-which.sh")
    ),
  }
)

def haskell_toolchain(
    name,
    version,
    tools,
    **kwargs):
  """Declare a compiler toolchain.

  Declares a compiler toolchain. You need at least one of these declared
  somewhere in your `BUILD` files for the other rules to work. Once
  declared, you then need to *register* the toolchain using
  `register_toolchain` in your `WORKSPACE` file (see example below).

  Haskell rules rely on some binary utilities, such as `ln` and `grep`. You
  can overwrite their locations by specifying argumentents such as
  `ln_location`, `grep_location`, and/or others, although it should be
  rarely needed.

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
