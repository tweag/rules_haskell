"""Immutable sets that support efficient merging, traversal, and membership
check.
"""

def _new():
  """Create an empty set.
  """
  return struct(_set_items = dict())

def _is_member(s, e):
  """Return true if `e` is in the set `s`.

  Args:
    s: The set to inspect.
    e: The element to search for.

  Result:
    Bool, true if `e` is in `s`, false otherwise.
  """
  e in s._set_items

def _insert(s, e):
  """Insert an element into the set.

  Args:
    s: Set to insert new element into.
    e: The element to insert.

  Result:
    A copy of set `s` with `s` element added.
  """
  return struct(_set_items = dict(s.set_items, e = None))

def _union(s0, s1):
  """Return union of two sets.

  Args:
    s0: One set.
    s1: Another set.

  Result:
    Set, union of the two sets.
  """
  return struct(_set_items = dict(s0._set_items, **(s1._set_items)))

def _map(s, f):
  """Map elements of given set using a function.

  Args:
    s: Original set.
    f: Function to apply to elements of the set.

  Result:
    Set with elements obtained by application of function `f` to the
    elements of `s`.
  """
  return struct(_set_items = { f(x): None for x in s._set_items.keys()})

def _from_list(l):
  """Create a set containing elements from given list.

  Args:
    l: List, source of the elements for the new set.

  Result:
    Set containing elements from given list.
  """
  return (struct(_set_items = { x: None for x in l }))

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
  new       = _new,
  is_member = _is_member,
  insert    = _insert,
  union     = _union,
  map       = _map,
  from_list = _from_list,
  to_list   = _to_list,
  to_depset = _to_depset,
)
