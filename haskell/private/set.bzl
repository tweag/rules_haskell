"""Immutable sets that support efficient merging, traversal, and membership
check.
"""

def _empty():
    """Create an empty set.

    Returns:
      set, new empty set.
    """
    return struct(_set_items = dict())

def _singleton(e):
    """Create a set with single element `e` inside.

    Args:
      e: The element to put in the set.

    Returns:
      set, new set.
    """
    r = dict()
    r[e] = None
    return struct(_set_items = r)

def _is_member(s, e):
    """Return true if `e` is in the set `s`.

    Args:
      s: The set to inspect.
      e: The element to search for.

    Result:
      Bool, true if `e` is in `s`, false otherwise.
    """
    return e in s._set_items

def _insert(s, e):
    """Insert an element into the set.

    Args:
      s: Set to insert new element into.
      e: The element to insert.

    Result:
      A copy of set `s` with `s` element added.
    """
    r = dict(s._set_items)
    r[e] = None
    return struct(_set_items = r)

def _mutable_insert(s, e):
    """The same as `set.insert`, but modifies the first argument in place.

    Args:
      s: Set to insert new element into.
      e: The element to insert.

    Result:
      set `s` with `s` element added.
    """
    s._set_items[e] = None
    return s

def _union(s0, s1):
    """Return union of two sets.

    Args:
      s0: One set.
      s1: Another set.

    Result:
      set, union of the two sets.
    """
    r = dict(s0._set_items)
    r.update(s1._set_items)
    return struct(_set_items = r)

def _mutable_union(s0, s1):
    """Modify set `s0` adding elements from `s1` to it.

    Args:
      s0: One set.
      s1: Another set.

    Result:
      set, union of the two sets.
    """
    s0._set_items.update(s1._set_items)
    return s0

def _map(s, f):
    """Map elements of given set using a function.

    Args:
      s: Original set.
      f: Function to apply to elements of the set.

    Result:
      set with elements obtained by application of function `f` to the
      elements of `s`.
    """
    return struct(_set_items = {f(x): None for x in s._set_items.keys()})

def _from_list(l):
    """Create a set containing elements from given list.

    Args:
      l: List, source of the elements for the new set.

    Result:
      set containing elements from given list.
    """
    return (struct(_set_items = {x: None for x in l}))

def _to_list(s):
    """Convert set into a list of its elements.

    Args:
      s: Set to convert.

    Returns:
      List of elements of the set.
    """
    return s._set_items.keys()

def _to_depset(s):
    """Similar to `set.to_list`, but produces a depset.

    Args:
      s: Set to convert.

    Returns:
      Depset of elements from the set.
    """
    return depset(_to_list(s))

set = struct(
    empty = _empty,
    singleton = _singleton,
    is_member = _is_member,
    insert = _insert,
    mutable_insert = _mutable_insert,
    union = _union,
    mutable_union = _mutable_union,
    map = _map,
    from_list = _from_list,
    to_list = _to_list,
    to_depset = _to_depset,
)
