"""Misc utilities."""

load(":set.bzl", "set")

def get_cc_defines(ctx):
  """
  """
  defines = set.empty()

  for dep in ctx.attr.deps:
    if hasattr(dep, "cc"):
      more_defines = dep.defines
      defines.mutable_union(defines, set.from_list(more_defines))

  return defines
