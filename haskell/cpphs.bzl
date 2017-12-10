"""cpphs file handling.
"""

load(":path_utils.bzl",
     "declare_compiled",
)

def cpphs(ctx):
  """Process all cpphs files into Haskell source files.

  Args:
    ctx: Rule context.
  """
  return [__process_cpphs_file(ctx, f) for f in ctx.files.cpphs]

def __process_cpphs_file(ctx, cpphs_file):
  """Process a single cpphs file.

  Args:
    ctx: Rule context.
    cpphs_file: cpphs file to process.
  """
  # Output a Haskell source file.
  hs_out = declare_compiled(ctx, cpphs_file, "hs")
  # Make all external dependency files available.
  external_files = depset([f for dep in ctx.attr.external_deps
                             for f in dep.files])
  # Add all directories of external dependencies to include dirs.
  include_directories = depset([f.dirname for f in external_files])

  # Build args for GHC to use cpphs.
  args = ctx.actions.args()
  args.add([
    "-E", "-cpp", "-pgmPcpphs", "-optP--cpp", "-x", "hs", "-o", hs_out,
    cpphs_file
  ])
  for include_dir in include_directories:
    args.add("-optP-I{0}".format(include_dir))

  ctx.actions.run(
    inputs = external_files + depset([cpphs_file]),
    outputs = [hs_out],
    use_default_shell_env = True,
    progress_message = "cpphs {0}".format(cpphs_file.basename),
    executable = "ghc",
    arguments = [args],
  )
  return hs_out
