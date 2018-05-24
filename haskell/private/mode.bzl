"""Compilation modes."""

def is_profiling_enabled(hs):
  """Check whether profiling mode is enabled.

  Args:
    hs: Haskell context.

  Returns:
    bool: True if the mode is enabled, False otherwise.
  """
  return hs.mode == "dbg"

def add_mode_options(hs, args):
  """Add mode options to the given args object.

  Args:
    hs: Haskell context.
    args: args object.

  Returns:
    None
  """
  if is_profiling_enabled(hs):
    args.add("-prof")
