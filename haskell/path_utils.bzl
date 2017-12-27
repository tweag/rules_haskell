"""Utilities for module and path manipulations."""

load("@bazel_skylib//:lib.bzl", "paths")

def path_to_module_path(ctx, hs_file):
  """Map a source file to a module name.

  some-workspace/some-package/src/Foo/Bar/Baz.hs => Foo/Bar/Baz

  Args:
    ctx: Rule context.
    hs_file: Haskell source file.
  """
  # Directory under which module hierarchy starts.
  pkg_dir = paths.join(ctx.label.workspace_root,
                       ctx.label.package,
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

def declare_compiled(ctx, src, ext, directory=None):
  """Given a Haskell-ish source file, declare its output.

  Args:
    ctx: Rule context.
    src: Haskell source file.
    ext: New extension.
    directory: Directory the new file should live in.
  """
  fp = paths.replace_extension(path_to_module_path(ctx, src), ext)
  fp_with_dir = fp if directory == None else paths.join(directory.basename, fp)
  return ctx.actions.declare_file(fp_with_dir)

def mk_name(ctx, name_prefix):
  """Make a target-unique name.

  Args:
    name_prefix: Template for the name.
  """
  return "{0}-{1}-{2}".format(name_prefix, ctx.attr.name, ctx.attr.version)
