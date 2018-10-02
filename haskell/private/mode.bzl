"""Compilation modes."""

def is_profiling_enabled(hs):
    """Check whether profiling mode is enabled.

    Args:
      hs: Haskell context.

    Returns:
      bool: True if the mode is enabled, False otherwise.
    """
    return False
    # return hs.mode == "dbg"
