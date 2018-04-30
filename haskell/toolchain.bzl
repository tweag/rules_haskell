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
  ghc_binaries = {}
  for tool in ctx.files.tools:
    if tool.basename in _GHC_BINARIES:
      ghc_binaries[tool.basename] = tool.path

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
  inputs = []
  inputs.extend(ctx.files.tools)
  inputs.extend(ctx.files._crosstool)


  targets_r = {
      "ar": ctx.host_fragments.cpp.ar_executable,
      "gcc": ctx.host_fragments.cpp.compiler_executable,
      "ld": ctx.host_fragments.cpp.ld_executable,
      "nm": ctx.host_fragments.cpp.nm_executable,
      "cpp": ctx.host_fragments.cpp.preprocessor_executable,
      "strip": ctx.host_fragments.cpp.strip_executable,
  } + ghc_binaries

  # If running on darwin but XCode is not installed (i.e., only the Command
  # Line Tools are available), then Bazel will make ar_executable point to
  # "/usr/bin/libtool".  Since we call ar directly, override it.
  # TODO: remove this if Bazel fixes its behavior.
  if targets_r["ar"].find("libtool") >= 0:
    targets_r["ar"] = "/usr/bin/ar"

  ar_runfiles = []

  # "xcrunwrapper.sh" is a Bazel-generated dependency of the `ar` program on macOS.
  xcrun_candidates = [f for f in ctx.files._crosstool if paths.basename(f.path) == "xcrunwrapper.sh"]
  if xcrun_candidates:
    xcrun = xcrun_candidates[0]
    ar_runfiles += [xcrun]
    targets_r["xcrunwrapper.sh"] = xcrun.path

  if ctx.attr.doctest != None:
    targets_r["doctest"] = ctx.file.doctest.path
    inputs.append(ctx.file.doctest)

  for target in targets_r:
    symlink = ctx.actions.declare_file(
      paths.join(visible_binaries, target)
    )
    symlink_target = targets_r[target]
    if not paths.is_absolute(symlink_target):

      symlink_target = paths.join("/".join([".."] * len(symlink.dirname.split("/"))), symlink_target)
    ctx.actions.run(
      inputs = inputs,
      outputs = [symlink],
      executable = "ln",
      # FIXME Currently this part of the process is not hermetic. This
      # should be adjusted when
      #
      # https://github.com/bazelbuild/bazel/issues/4681
      #
      # is implemented.
      use_default_shell_env = True,
      arguments = [
        "-s",
        symlink_target,
        symlink.path,
      ],
    )
    if target == "xcrunwrapper.sh":
      ar_runfiles += [symlink]

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


  tools_struct_args = ({tool.basename.replace("-", "_"): tool
                       for tool in set.to_list(symlinks)}
                       + {"ar_runfiles": ar_runfiles})

  return [
    platform_common.ToolchainInfo(
      name = ctx.label.name,
      tools = struct(**tools_struct_args),
      # All symlinks are guaranteed to be in the same directory so we just
      # provide directory name of the first one (the collection cannot be
      # empty). The rest of the program may rely consider visible_bin_path
      # as the path to visible binaries, without recalculations.
      visible_bin_path = set.to_list(symlinks)[0].dirname,
      is_darwin = ctx.attr.is_darwin,
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
    "tools": attr.label(
      doc = "GHC and executables that come with it",
      mandatory = True,
    ),
    "doctest": attr.label(
      doc = "Doctest executable",
      allow_single_file = True,
    ),
    "version": attr.string(mandatory = True),
    "is_darwin":  attr.bool(mandatory = True),
    "_ghc_version_check": attr.label(
      allow_single_file = True,
      default = Label("@io_tweag_rules_haskell//haskell:ghc-version-check.sh")
    ),
    "_make_bin_symlink_which": attr.label(
      allow_single_file = True,
      default = Label("@io_tweag_rules_haskell//haskell:make-bin-symlink-which.sh")
    ),
    "_crosstool": attr.label(
        default = Label("//tools/defaults:crosstool")
    ),
  }
)

def haskell_toolchain(
    name,
    version,
    tools,
    **kwargs):
  """Declare a compiler toolchain.

  You need at least one of these declared somewhere in your `BUILD` files
  for the other rules to work. Once declared, you then need to *register*
  the toolchain using `register_toolchain` in your `WORKSPACE` file (see
  example below).

  Example:
    ```bzl
    haskell_toolchain(
        name = "ghc",
        version = '1.2.3'
        tools = ["@sys_ghc//:bin"],
        doctest = "@doctest//:bin", # optional
    )
    ```

    where `@ghc` is an external repository defined in the `WORKSPACE`,
    e.g. using:

    ```bzl
    nixpkgs_package(
        name = 'sys_ghc',
        attribute_path = 'haskell.compiler.ghc822'
    )

    register_toolchain("//:ghc")
    ```

    similarly for `@doctest`:

    ```bzl
    nixpkgs_package(
        name = "doctest",
        attribute_path = "haskell.packages.ghc822.doctest",
    )
    ```
  """
  impl_name = name + "-impl"
  _haskell_toolchain(
    name = impl_name,
    version = version,
    tools = tools,
    visibility = ["//visibility:public"],
    is_darwin = select({
        "@bazel_tools//src/conditions:darwin": True,
        "//conditions:default": False,
    }),
    **kwargs
  )

  native.toolchain(
    name = name,
    toolchain_type = "@io_tweag_rules_haskell//haskell:toolchain",
    toolchain = ":" + impl_name,
    exec_compatible_with = [
      '@bazel_tools//platforms:x86_64'],
    target_compatible_with = [
      '@bazel_tools//platforms:x86_64'],
  )
  # TODO: perhaps make a toolchain for darwin?
