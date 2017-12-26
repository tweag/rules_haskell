"""Utilities for module and path manipulations.
"""

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

def drop_path_prefix(path_prefix, full_path):
  """Drop path_prefix path prefix from full_path if it exists.

  drop_path_prefix("foo/bar", "foo/bar/baz") => "baz"
  drop_path_prefix("", "/foo") => "foo"
  drop_path_prefix("foo/mid", "foo/middle/bar") => "dle/bar"
  drop_path_prefix("nomatch", "some/path") => "some/path"
  drop_path_prefix("nomatch", "/some/path") => "/some/path"

  Args:
    path_prefix: Prefix to drop from full_path.
    full_path: Path to drop path_prefix from.

  """
  # Check if prefix matches
  if path_prefix == full_path[:len(path_prefix)]:
    new_path = full_path[len(path_prefix):]
    # We cut off the prefix and found a leading path separator, drop
    # it as it was necessary with the prefix. Most likely.
    if new_path[:1] == '/':
      return new_path[1:]
    else:
      return new_path
  else:
    return full_path

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
  path_no_prefix = drop_path_prefix(pkg_dir, hs_file.path)
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

def replace_ext(file_path, new_ext):
  """Replace an extension in a path with the given one.

  replace_ext("foo/bar.hs", "o") => "foo/bar.o"
  replace_ext("foo/bar.hs", "") => "foo/bar"
  replace_ext("foo/bar", "") => "foo/bar"
  replace_ext("foo/bar", "hs") => "foo/bar.hs"
  replace_ext("foo/bar.hs", ".o") => "foo/bar.o"

  Args:
    file_path: Path to replace extension in.
    new_ext: Extension give to the new path.
  """

  dot_pos = file_path.rfind(".")
  file_path_no_ext = file_path if dot_pos < 0 else file_path[:dot_pos]
  if new_ext != None and new_ext != "":
    if new_ext[:1] == ".":
      # If user passed ext with dot, don't add one ourselves.
      return "{0}{1}".format(file_path_no_ext, new_ext)
    else:
      return "{0}.{1}".format(file_path_no_ext, new_ext)
  else:
    return file_path_no_ext

def declare_compiled(ctx, src, ext, directory=None):
  """Given a Haskell-ish source file, declare its output.

  Args:
    ctx: Rule context.
    src: Haskell source file.
    ext: New extension.
    directory: Directory the new file should live in.
  """
  fp = replace_ext(path_to_module_path(ctx, src), ext)
  fp_with_dir = fp if directory == None else path_append(directory.basename, fp)
  return ctx.actions.declare_file(fp_with_dir)

def mk_name(ctx, name_prefix):
  """Make a target-unique name.

  Args:
    name_prefix: Template for the name.
  """
  return "{0}-{1}-{2}".format(name_prefix, ctx.attr.name, ctx.attr.version)
