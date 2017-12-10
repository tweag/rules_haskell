"""Utilities for module and path manipulations.
"""

def path_append(p1, p2):
  """Append the two given paths with / separator but without creating
  spurious separators if either path is empty or already has a
  separator present. It does not try to normalise the path. None is
  treated as empty path.

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

def drop_path_prefix(pathPrefix, fullPath):
  """Drop pathPrefix path prefix from fullPath if it exists.

  drop_path_prefix("foo/bar", "foo/bar/baz") => "baz"
  drop_path_prefix("", "/foo") => "foo"
  drop_path_prefix("foo/mid", "foo/middle/bar") => "dle/bar"
  drop_path_prefix("nomatch", "some/path") => "some/path"
  drop_path_prefix("nomatch", "/some/path") => "/some/path"

  Args:
    pathPrefix: Prefix to drop from fullPath.
    fullPath: Path to drop pathPrefix from.

  """
  # Check if prefix matches
  if pathPrefix == fullPath[:len(pathPrefix)]:
    newPath = fullPath[len(pathPrefix):]
    # We cut off the prefix and found a leading path separator, drop
    # it as it was necessary with the prefix. Most likely.
    if newPath[:1] == '/':
      return newPath[1:]
    else:
      return newPath
  else:
    return fullPath

def path_to_module_path(ctx, hsFile):
  """Given Haskell source file path, get the module hierarchy without the
    extension.

  some-workspace/some-package/src/Foo/Bar/Baz.hs => Foo/Bar/Baz

  Args:
    ctx: Rule context.
    hsFile: Haskell source file.
  """
  # Directory under which module hierarchy starts.
  pkgDir = path_append(path_append(ctx.label.workspace_root, ctx.label.package),
                       ctx.attr.sourceDir)
  # Module path without the workspace and source directories, just
  # relevant hierarchy.
  noPrefixPath = drop_path_prefix(pkgDir, hsFile.path)
  # Drop extension.
  return noPrefixPath[:noPrefixPath.rfind(".")]

def path_to_module(ctx, hsFile):
  """Given Haskell source file path, turn it to a module name.

  some-workspace/some-package/src/Foo/Bar/Baz.hs => Foo.Bar.Baz

  Args:
    ctx: Rule context.
    hsFile: Haskell source file.
  """
  return path_to_module_path(ctx, hsFile).replace('/', '.')

def get_object_suffix():
  """Get the object file suffix that GHC expects for this mode of compilation.
  """
  return "o"

def get_dyn_object_suffix():
  """Get the dynamic object file suffix.
  """
  return "dyn_o"

def get_interface_suffix():
  """Get the interface file suffix that GHC expects for this mode of
  compilation.
  """
  return "hi"

def get_dyn_interface_suffix():
  """Get the dynamic interface file suffix that GHC expects for this
  mode of compilation.
  """
  return "dyn_hi"


def replace_ext(filePath, newExt):
  """Replace an extension in a path with the given one.

  replace_ext("foo/bar.hs", "o") => "foo/bar.o"
  replace_ext("foo/bar.hs", "") => "foo/bar"
  replace_ext("foo/bar", "") => "foo/bar"
  replace_ext("foo/bar", "hs") => "foo/bar.hs"
  replace_ext("foo/bar.hs", ".o") => "foo/bar.o"

  Args:
    filePath: Path to replace extension in.
    newExt: Extension give to the new path.
  """

  dotPos = filePath.rfind(".")
  noExt = filePath if dotPos < 0 else filePath[:dotPos]
  if newExt != None and newExt != "":
    if newExt[:1] == ".":
      # If user passed ext with dot, don't add one ourselves.
      return "{0}{1}".format(noExt, newExt)
    else:
      return "{0}.{1}".format(noExt, newExt)
  else:
    return noExt

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
