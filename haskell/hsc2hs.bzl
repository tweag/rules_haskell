"""hsc file handling."""

load(":path_utils.bzl",
  "declare_compiled",
  "module_unique_name",
)

load(":tools.bzl", "get_hsc2hs")
load(":cc.bzl", "cc_headers")
load("@bazel_skylib//:lib.bzl", "paths")
load(":tools.bzl", "get_compiler")

def hsc_to_hs(ctx):
  """Process all hsc files into Haskell source files.

  Args:
    ctx: Rule context.

  Returns:
    list of File: New Haskell source files to use.
  """
  ghc_defs_dump = _make_ghc_defs_dump(ctx)
  sources = []
  for f in ctx.files.srcs:
    if f.extension == "hsc":
      sources.append(_process_hsc_file(ctx, ghc_defs_dump, f))
    else:
      sources.append(f)
  return sources

def _make_ghc_defs_dump(ctx):
  """Generate a file containing GHC default pre-processor definitions.

  Args:
    ctx: Rule context.

  Returns:
    File: The file with GHC definitions.
  """
  raw_filename      = "ghc-defs-dump-{0}-{1}.hs".format(ctx.attr.name, ctx.attr.version)
  dummy_src         = ctx.actions.declare_file(raw_filename)
  ghc_defs_dump_raw = ctx.actions.declare_file(paths.replace_extension(raw_filename, ".hspp"))
  ghc_defs_dump     = ctx.actions.declare_file(paths.replace_extension(raw_filename, ".h"))

  ctx.actions.write(dummy_src, "")
  args = ctx.actions.args()
  args.add([
    "-E",
    "-optP-dM",
    "-cpp",
    dummy_src.path,
  ])

  ctx.actions.run(
    inputs = [dummy_src],
    outputs = [ghc_defs_dump_raw],
    executable = get_compiler(ctx),
    arguments = [args],
  )

  ctx.actions.run(
    inputs = [ghc_defs_dump_raw],
    outputs = [ghc_defs_dump],
    executable = ctx.file._ghc_defs_cleanup,
    use_default_shell_env = True,
    arguments  = [
      ghc_defs_dump_raw.path,
      ghc_defs_dump.path,
    ],
  )

  return ghc_defs_dump

def _process_hsc_file(ctx, ghc_defs_dump, hsc_file):
  """Process a single hsc file.

  Args:
    ctx: Rule context.
    ghc_defs_dump: File with GHC definitions.
    hsc_file: hsc file to process.

  Returns:
    File: Haskell source file created by processing hsc_file.
  """

  hsc_output_dir = ctx.actions.declare_directory(
    module_unique_name(ctx, hsc_file, "hsc_processed")
  )
  args = ctx.actions.args()

  # Output a Haskell source file.
  hs_out = declare_compiled(ctx, hsc_file, ".hs", directory=hsc_output_dir)
  args.add([hsc_file, "-o", hs_out])

  # Bring in scope the header files of dependencies, if any.
  hdrs, include_args = cc_headers(ctx)
  args.add(include_args)
  args.add("-I{0}".format(ghc_defs_dump.dirname))
  args.add("-i{0}".format(ghc_defs_dump.basename))

  ctx.actions.run(
    inputs = depset(transitive =
                    [depset(hdrs), depset([hsc_file, ghc_defs_dump])]),
    outputs = [hs_out, hsc_output_dir],
    use_default_shell_env = True,
    progress_message = "hsc2hs {0}".format(hsc_file.basename),
    executable = get_hsc2hs(ctx),
    arguments = [args],
  )
  return hs_out
