"""Compilation modes."""

def _is_mode_enabled(ctx, mode):
  """Check whether a compilation mode is enabled.

  Args:
    ctx: Rule context.
    mode: Mode to check.

  Returns:
    bool: True if the mode is enabled, False otherwise.
  """
  return ctx.var["COMPILATION_MODE"] == mode;

def is_profiling_enabled(ctx):
  """Check whether profiling mode is enabled.

  Args:
    ctx: Rule context.

  Returns:
    bool: True if the mode is enabled, False otherwise.
  """
  return _is_mode_enabled(ctx, "dbg")

def add_mode_options(ctx, args):
  """Add mode options to the given args object.

  Args:
    ctx: Rule context.
    args: args object.

  Returns:
    None
  """
  if is_profiling_enabled(ctx):
    args.add("-prof")
