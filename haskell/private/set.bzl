"""Immutable sets that support efficient merging, traversal, and membership
check. 
"""

load("@bazel_skylib//lib:sets.bzl", "sets")

def _mutable_union(s0, s1):
    """Modify set `s0` adding elements from `s1` to it.
    This is soon to be upstreamed to Skylib and removed from this file. See
    <https://github.com/tweag/rules_haskell/pull/1834> and
    <https://github.com/bazelbuild/bazel-skylib/pull/415> for more information.

    Args:
      s0: One set.
      s1: Another set.

    Result:
      set, union of the two sets.
    """

    s0._values.update(s1._values)
    return s0

def _mutable_difference(s0, s1):
    """Modify set `s0` removing elements from `s1` from it.
    This is soon to be upstreamed to Skylib and removed from this file. See
    <https://github.com/tweag/rules_haskell/pull/1834> and
    <https://github.com/bazelbuild/bazel-skylib/pull/415> for more information.

    Args:
      s0: One set.
      s1: Another set.

    Result:
      set, difference of the two sets.
    """

    for item in s1._values.keys():
        s0._values.pop(item)
    return s0

def _to_depset(s):
    """Similar to `sets.to_list`, but produces a depset.
    Args:
      s: Set to convert.
    Returns:
      Depset of elements from the set.
    """
    return depset(sets.to_list(s))

set = struct(
    mutable_union = _mutable_union,
    mutable_difference = _mutable_difference,
    to_depset = _to_depset,
)
