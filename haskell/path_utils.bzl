"""Utilities for module and path manipulations.
"""

load("@bazel_skylib//:lib.bzl", "paths")

def path_append(p1, p2):
  """Concatenate two paths without creating spurious separators.

  This function does not try to normalise the path. None is treated as
  empty path.

  path_append("foo", "bar") => "foo/bar"
  path_append("foo", "/bar") => "foo/bar"
  path_append("foo/", "bar") => "foo/bar"
  path_append("foo/", "/bar") => "foo//bar"
  path_append("foo", "") => "foo"
  path_append("foo/", "") => "foo/"
  path_append("", "bar") => "bar"
  path_append("", "/bar") => "/bar"
  path_append("", "") => ""

  Args:
    p1: Left side of the path.
    p2: Right side of the path.
  """
  # Front empty
  if p1 == "" or p1 == None:
    return p2
  # Back empty
  elif p2 == "" or p2 == None:
    return p1
  # Neither empty, but if there's a slash at either end of p1 or start
  # of p2, just append. If there's one at both, too bad.
  elif p1[:-1] == '/' or p2[0] == '/':
    return p1 + p2
  # No slash at join point add one.
  else:
    return "{0}/{1}".format(p1, p2)

def path_to_module_path(ctx, hs_file):
  """Map a source file to a module name.

  some-workspace/some-package/src/Foo/Bar/Baz.hs => Foo/Bar/Baz

  Args:
    ctx: Rule context.
    hs_file: Haskell source file.
  """
  # Directory under which module hierarchy starts.
  pkg_dir = path_append(path_append(ctx.label.workspace_root, ctx.label.package),
                        ctx.attr.src_strip_prefix)
  # Module path without the workspace and source directories, just
  # relevant hierarchy.
  path_no_prefix = paths.relativize(hs_file.path, pkg_dir)
  # Drop extension.
  return path_no_prefix[:path_no_prefix.rfind(".")]

def path_to_module(ctx, hs_file):
  """Given Haskell source file path, turn it to a module name.

  some-workspace/some-package/src/Foo/Bar/Baz.hs => Foo.Bar.Baz

  Args:
    ctx: Rule context.
    hs_file: Haskell source file.
  """
  return path_to_module_path(ctx, hs_file).replace('/', '.')

def get_object_suffix():
  """Get the object file suffix that GHC expects for this mode of compilation."""
  return "o"

def get_dyn_object_suffix():
  """Get the dynamic object file suffix."""
  return "dyn_o"

def get_interface_suffix():
  """Get the interface file suffix that GHC expects for this mode of compilation."""
  return "hi"

def get_dyn_interface_suffix():
  """Same as get_interface_suffix(), but for dynamic linking."""
  return "dyn_hi"

def declare_compiled(ctx, src, ext, directory=None):
  """Given a Haskell-ish source file, declare its output.

  Args:
    ctx: Rule context.
    src: Haskell source file.
    ext: New extension.
    directory: Directory the new file should live in.
  """
  fp = paths.replace_extension(path_to_module_path(ctx, src), "." + ext)
  fp_with_dir = fp if directory == None else path_append(directory.basename, fp)
  return ctx.actions.declare_file(fp_with_dir)

def mk_name(ctx, name_prefix):
  """Make a target-unique name.

  Args:
    name_prefix: Template for the name.
  """
  return "{0}-{1}-{2}".format(name_prefix, ctx.attr.name, ctx.attr.version)
