def typecheck_stackage_extradeps(extra_deps):
    """Check that the extra_deps field of a stackage rule is well typed.
    If not, fail.

    Args:
      extra_deps: The value of the extra_deps field of a stackage rule
    """
    if not extra_deps:
        return
    if type(extra_deps) != "dict":
        fail("stack_snapshot extra_deps requires a dict from dependency name to list of targets, but was given: {}".format(type(extra_deps)))
    for extra_deps_key in extra_deps.keys():
        if type(extra_deps_key) != "string":
            fail("stack_snapshot extra_deps's dict requires string keys, but key \"{}\" has type {}".format(extra_deps_key, type(extra_deps_key)))
    for extra_deps_value in extra_deps.values():
        if type(extra_deps_value) != "list":
            fail("stack_snapshot extra_deps's dict requires list values, but value \"{}\" has type {}".format(extra_deps_value, type(extra_deps_value)))
