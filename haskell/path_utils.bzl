"""Utilities for module and path manipulations."""

load("@bazel_skylib//:lib.bzl", "paths")

def path_to_module_path(ctx, hs_file, prefix=None):
  """Map a source file to a module name.

  some-workspace/some-package/src/Foo/Bar/Baz.hs => Foo/Bar/Baz

  Args:
    ctx: Rule context.
    hs_file: Haskell source file.
    prefix: Prefix to strip. ctx.attr.src_strip_prefix used if not set.

  Returns:
    string: Module part of hs_file. See example above.
  """
  strip_prefix = prefix if prefix != None else ctx.attr.src_strip_prefix

  # Directory under which module hierarchy starts.
  pkg_dir = paths.join(ctx.label.workspace_root,
                       ctx.label.package,
                       strip_prefix)
  # Module path without the workspace and source directories, just
  # relevant hierarchy.
  path_no_prefix = paths.relativize(hs_file.path, pkg_dir)
  # Drop extension.
  return path_no_prefix[:path_no_prefix.rfind(".")]

def path_to_module(ctx, hs_file, sep='.', prefix=None):
  """Given Haskell source file path, turn it to a module name.

  some-workspace/some-package/src/Foo/Bar/Baz.hs => Foo.Bar.Baz

  Args:
    ctx: Rule context.
    hs_file: Haskell source file.
    sep: Separator to use. By default `.`.
    prefix: Prefix to strip. ctx.attr.src_strip_prefix used if not set.

  Returns:
    string: Haskell module name. See example above.
  """
  return path_to_module_path(ctx, hs_file, prefix = prefix).replace('/', sep)

def declare_compiled(ctx, src, ext, directory=None):
  """Given a Haskell-ish source file, declare its output.

  Args:
    ctx: Rule context.
    src: Haskell source file.
    ext: New extension.
    directory: Directory the new file should live in.

  Returns:
    File: Declared output file living in `directory` with given `ext`.
  """
  fp = paths.replace_extension(path_to_module_path(ctx, src), ext)
  fp_with_dir = fp if directory == None else paths.join(directory.basename, fp)
  return ctx.actions.declare_file(fp_with_dir)

def mk_name(ctx, name_prefix):
  """Make a target-unique name.

  `name_prefix` is made target-unique by adding rule name and target
  version suffix to it. This means that given two different rules, the
  same `name_prefix` is distinct. Note that this is does not
  disambiguate two names within the same rule. Given a haskell_library
  with name foo and version 0.1.0, you could expect:

  mk_name(ctx, "libdir") => "libdir-foo-0.1.0"

  This allows two rules using same name_prefix being built in same
  environment to avoid name clashes of their output files and
  directories.

  Args:
    ctx: Rule context.
    name_prefix: Template for the name.

  Returns:
    string: Target-unique name_prefix.

  """
  return "{0}-{1}-{2}".format(name_prefix, ctx.attr.name, ctx.attr.version)

def mk_module_name(ctx, source_name, name_prefix):
  """
  Make a target-unique and `source_name`-unique name.

  This is quite similar to `mk_name` but also uses a path built from
  `source_name` to prevent clashes with other names produced using the same
  `name_prefix`.

  Args:
    ctx: Rule context.
    source_name: Source file name.
    name_prefix: Template for the name.

  Returns:
    string: Target- and source-unique name.
  """
  return "{0}-{1}-{2}-{3}__".format(
    name_prefix,
    ctx.attr.name,
    ctx.attr.version,
    path_to_module_path(ctx, source_name),
  )
