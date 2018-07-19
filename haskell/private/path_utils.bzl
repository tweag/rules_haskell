"""Utilities for module and path manipulations."""

load(":private/set.bzl", "set")
load("@bazel_skylib//:lib.bzl", "paths")

def module_name(hs, f, rel_path = None):
    """Given Haskell source file path, turn it into a dot-separated module name.

    module_name(
      hs,
      "some-workspace/some-package/src/Foo/Bar/Baz.hs",
    ) => "Foo.Bar.Baz"

    Args:
      hs:  Haskell context.
      f:   Haskell source file.
      rel_path: Explicit relative path from import root to the module, or None
        if it should be deduced.

    Returns:
      string: Haskell module name.
    """

    rpath = rel_path

    if not rpath:
        rpath = _rel_path_to_module(hs, f)

    (hsmod, _) = paths.split_extension(rpath.replace("/", "."))
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
        module_name(hs, source_file),
    )

def declare_compiled(hs, src, ext, directory = None, rel_path = None):
    """Given a Haskell-ish source file, declare its output.

    Args:
      hs: Haskell context.
      src: Haskell source file.
      ext: New extension.
      directory: String, directory prefix the new file should live in.
      rel_path: Explicit relative path from import root to the module, or None
        if it should be deduced.

    Returns:
      File: Declared output file living in `directory` with given `ext`.
    """

    rpath = rel_path

    if not rpath:
        rpath = _rel_path_to_module(hs, src)

    fp = paths.replace_extension(rpath, ext)
    fp_with_dir = fp if directory == None else paths.join(directory, fp)

    return hs.actions.declare_file(fp_with_dir)

def get_external_libs_path(libs, prefix = None):
    """Return a String value for using as LD_LIBRARY_PATH or similar.

    Args:
      libs: Set of File: the libs that should be available.
      prefix: String, an optional prefix to add to every path.

    Returns:
      String: paths to the given libs separated by ":".
    """
    r = []

    for lib in set.to_list(set.map(libs, _get_external_lib_path)):
        if prefix:
            lib = paths.join(prefix, lib)

        r.append(lib)

    return ":".join(r)

def _get_external_lib_path(lib):
    return paths.dirname(lib.path)

def get_lib_name(lib):
    """Return name of library by dropping extension and "lib" prefix.

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

# TODO Consider merging with paths.relativize. See
# https://github.com/bazelbuild/bazel-skylib/pull/44.
def _truly_relativize(target, relative_to):
    """Return a relative path to `target` from `relative_to`.

    Args:
      target: string, path to directory we want to get relative path to.
      relative_to: string, path to directory from which we are starting.

    Returns:
      string: relative path to `target`.
    """
    t_pieces = target.split("/")
    r_pieces = relative_to.split("/")
    common_part_len = 0

    for tp, rp in zip(t_pieces, r_pieces):
        if tp == rp:
            common_part_len += 1
        else:
            break

    result = [".."] * (len(r_pieces) - common_part_len)
    result += t_pieces[common_part_len:]

    return "/".join(result)

def ln(hs, target, link, extra_inputs = depset()):
    """Create a symlink to target.

    Args:
      hs: Haskell context.
      extra_inputs: extra phony dependencies of symlink.

    Returns:
      None
    """
    relative_target = _truly_relativize(target.path, link.dirname)
    hs.actions.run_shell(
        inputs = depset([target], transitive = [extra_inputs]),
        outputs = [link],
        mnemonic = "Symlink",
        command = "ln -s {target} {link}".format(
            target = relative_target,
            link = link.path,
        ),
        use_default_shell_env = True,
    )
