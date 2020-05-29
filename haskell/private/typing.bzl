def typecheck_stackage_extradeps(extra_deps, field):
    """Check that the extra_deps or setup_deps field of a stackage rule is well typed.
    If not, fail.

    Args:
      extra_deps: The value of the extra_deps or setup_deps field of a stackage rule
      field: The field name
    """
    if not extra_deps:
        return
    if type(extra_deps) != "dict":
        fail("stack_snapshot {} requires a dict from dependency name to list of targets, but was given: {}".format(field, type(extra_deps)))
    for extra_deps_key in extra_deps.keys():
        if type(extra_deps_key) != "string":
            fail("stack_snapshot {}'s dict requires string keys, but key \"{}\" has type {}".format(field, extra_deps_key, type(extra_deps_key)))
    for extra_deps_value in extra_deps.values():
        if type(extra_deps_value) != "list":
            fail("stack_snapshot {}'s dict requires list values, but value \"{}\" has type {}".format(field, extra_deps_value, type(extra_deps_value)))
