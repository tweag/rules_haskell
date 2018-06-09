"""Utilities for module and path manipulations."""

load(":private/set.bzl", "set")
load("@bazel_skylib//:lib.bzl", "paths")

def module_name(hs, f):
  """Given Haskell source file path, turn it into a dot-separated module name.

  module_name(
    hs,
    "some-workspace/some-package/src/Foo/Bar/Baz.hs",
  ) => "Foo.Bar.Baz"

  Args:
    hs:  Haskell context.
    f:   Haskell source file.

  Returns:
    string: Haskell module name.
  """
  (hsmod, _) = paths.split_extension(
    _rel_path_to_module(hs, f).replace('/', '.')
  )
  return hsmod

def target_unique_name(hs, name_prefix):
  """Make a target-unique name.

  `name_prefix` is made target-unique by adding a rule name
  suffix to it. This means that given two different rules, the same
  `name_prefix` is distinct. Note that this is does not disambiguate two
  names within the same rule. Given a haskell_library with name foo
  you could expect:

  target_unique_name(hs, "libdir") => "libdir-foo"

  This allows two rules using same name_prefix being built in same
  environment to avoid name clashes of their output files and directories.

  Args:
    hs:          Haskell context.
    name_prefix: Template for the name.

  Returns:
    string: Target-unique name_prefix.
  """
  return "{0}-{1}".format(name_prefix, hs.name)

def module_unique_name(hs, source_file, name_prefix):
  """Make a target- and module- unique name.

  module_unique_name(
    hs,
    "some-workspace/some-package/src/Foo/Bar/Baz.hs",
    "libdir"
  ) => "libdir-foo-Foo.Bar.Baz"

  This is quite similar to `target_unique_name` but also uses a path built
  from `source_file` to prevent clashes with other names produced using the
  same `name_prefix`.

  Args:
    hs:          Haskell context.
    source_file: Source file name.
    name_prefix: Template for the name.

  Returns:
    string: Target- and source-unique name.
  """
  return "{0}-{1}".format(
    target_unique_name(hs, name_prefix),
    module_name(hs, source_file)
  )

def declare_compiled(hs, src, ext, directory=None):
  """Given a Haskell-ish source file, declare its output.

  Args:
    hs: Haskell context.
    src: Haskell source file.
    ext: New extension.
    directory: String, directory prefix the new file should live in.

  Returns:
    File: Declared output file living in `directory` with given `ext`.
  """
  fp = paths.replace_extension(_rel_path_to_module(hs, src), ext)
  fp_with_dir = fp if directory == None else paths.join(directory, fp)
  return hs.actions.declare_file(fp_with_dir)

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

def get_lib_name(lib):
  """Return name of library by dropping extension and \"lib\" prefix.

  Args:
    lib: The library File.

  Returns:
    String: name of library.
  """

  base = lib.basename[3:] if lib.basename[:3] == "lib" else lib.basename
  n = base.find(".so.")
  end = paths.replace_extension(base, "") if n == -1 else base[:n]
  return end

def _rel_path_to_module(hs, f):
  """Make given file name relative to the directory where the module hierarchy
  starts.

  _rel_path_to_module(
    "some-workspace/some-package/src/Foo/Bar/Baz.hs"
  ) => "Foo/Bar/Baz.hs"

  Args:
    hs:  Haskell context.
    f:   Haskell source file.

  Returns:
    string: Relative path to module file.
  """
  # If it's a generated file, strip off the bin or genfiles prefix.
  path = f.path
  if path.startswith(hs.bin_dir.path):
    path = paths.relativize(path, hs.bin_dir.path)
  elif path.startswith(hs.genfiles_dir.path):
    path = paths.relativize(path, hs.genfiles_dir.path)

  return paths.relativize(path, hs.src_root)
