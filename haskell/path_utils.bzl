"""Utilities for module and path manipulations."""

load(":set.bzl", "set")
load("@bazel_skylib//:lib.bzl", "paths", "shell")

def module_name(ctx, f):
  """Given Haskell source file path, turn it into a dot-separated module name.

  module_name(
    ctx,
    "some-workspace/some-package/src/Foo/Bar/Baz.hs",
  ) => "Foo.Bar.Baz"

  Args:
    ctx: Rule context.
    f:   Haskell source file.

  Returns:
    string: Haskell module name.
  """
  return _drop_extension(_rel_path_to_module(ctx, f).replace('/', '.'))

def target_unique_name(ctx, name_prefix):
  """Make a target-unique name.

  `name_prefix` is made target-unique by adding rule name and target version
  suffix to it. This means that given two different rules, the same
  `name_prefix` is distinct. Note that this is does not disambiguate two
  names within the same rule. Given a haskell_library with name foo and
  version 0.1.0, you could expect:

  target_unique_name(ctx, "libdir") => "libdir-foo-0.1.0"

  This allows two rules using same name_prefix being built in same
  environment to avoid name clashes of their output files and directories.

  Args:
    ctx:         Rule context.
    name_prefix: Template for the name.

  Returns:
    string: Target-unique name_prefix.
  """
  return "{0}-{1}-{2}".format(name_prefix, ctx.attr.name, ctx.attr.version)

def module_unique_name(ctx, source_file, name_prefix):
  """Make a target- and module- unique name.

  module_unique_name(
    ctx,
    "some-workspace/some-package/src/Foo/Bar/Baz.hs",
    "libdir"
  ) => "libdir-foo-0.1.0-Foo.Bar.Baz"

  This is quite similar to `target_unique_name` but also uses a path built
  from `source_file` to prevent clashes with other names produced using the
  same `name_prefix`.

  Args:
    ctx:         Rule context.
    source_file: Source file name.
    name_prefix: Template for the name.

  Returns:
    string: Target- and source-unique name.
  """
  return "{0}-{1}".format(
    target_unique_name(ctx, name_prefix),
    module_name(ctx, source_file)
  )

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
  fp = paths.replace_extension(_rel_path_to_module(ctx, src), ext)
  fp_with_dir = fp if directory == None else paths.join(directory.basename, fp)
  return ctx.actions.declare_file(fp_with_dir)

def import_hierarchy_root(ctx):
  """Return relative path to root of module hierarchy.

  Args:
    ctx: Rule context.

  Returns:
    string: Relative path to root of module hierarchy.
  """
  return paths.join(
    ctx.label.workspace_root,
    ctx.label.package,
    # Since the src_strip_prefix attribute is always present in rule
    # attributes, if it's not there, the function is called from aspect
    # implementation and so we can access ctx.rule.attr.src_strip_prefix.
    ctx.attr.src_strip_prefix if hasattr(ctx.attr, "src_strip_prefix")
                              else ctx.rule.attr.src_strip_prefix
  )

def get_external_libs_path(libs):
  """Return a String value for using as LD_LIBRARY_PATH or similar.

  Args:
    libs: Set of File: the libs that should be available.

  Returns:
    String: paths to the given libs separated by \":\".
  """
  return ":".join(set.to_list(set.map(libs, _get_external_lib_path)))

def _get_external_lib_path(lib):
  return paths.dirname(lib.path)

def _rel_path_to_module(ctx, f):
  """Make given file name relative to the directory where the module hierarchy
  starts.

  _rel_path_to_module(
    "some-workspace/some-package/src/Foo/Bar/Baz.hs"
  ) => "Foo/Bar/Baz.hs"

  Args:
    ctx: Rule context.
    f:   Haskell source file.

  Returns:
    string: Relative path to module file.
  """
  # If it's a generated file, strip off the bin or genfiles prefix.
  path = f.path
  if path.startswith(ctx.bin_dir.path):
    path = paths.relativize(path, ctx.bin_dir.path)
  elif path.startswith(ctx.genfiles_dir.path):
    path = paths.relativize(path, ctx.genfiles_dir.path)

  return paths.relativize(path, import_hierarchy_root(ctx))

def _drop_extension(f):
  """Drop extension for a given file name.

  _drop_extension("Foo/Bar/Baz.hs") => "Foo/Bar/Baz"

  Args:
    f: File path, possibly ending with an extension.

  Returns:
    string: `f` with extension removed.
  """
  return paths.split_extension(f)[0]
