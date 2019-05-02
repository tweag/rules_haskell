"""Utilities for module and path manipulations."""

load("@bazel_skylib//lib:paths.bzl", "paths")
load(":private/set.bzl", "set")

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

def make_path(libs, prefix = None, sep = None):
    """Return a string value for using as LD_LIBRARY_PATH or similar.

    Args:
      libs: List of library files that should be available
      prefix: String, an optional prefix to add to every path.
      sep: String, the path separator, defaults to ":".

    Returns:
      String: paths to the given library directories separated by ":".
    """
    r = set.empty()

    sep = sep if sep else ":"

    for lib in libs:
        lib_dir = paths.dirname(lib.path)
        if prefix:
            lib_dir = paths.join(prefix, lib_dir)

        set.mutable_insert(r, lib_dir)

    return sep.join(set.to_list(r))

def darwin_convert_to_dylibs(hs, libs):
    """Convert .so dynamic libraries to .dylib.

    Bazel's cc_library rule will create .so files for dynamic libraries even
    on MacOS. GHC's builtin linker, which is used during compilation, GHCi,
    or doctests, hard-codes the assumption that all dynamic libraries on MacOS
    end on .dylib. This function serves as an adaptor and produces symlinks
    from a .dylib version to the .so version for every dynamic library
    dependencies that does not end on .dylib.

    Args:
      hs: Haskell context.
      libs: List of library files dynamic or static.

    Returns:
      List of library files where all dynamic libraries end on .dylib.
    """
    lib_prefix = "_dylibs"
    new_libs = []
    for lib in libs:
        if is_shared_library(lib) and lib.extension != "dylib":
            dylib_name = paths.join(
                target_unique_name(hs, lib_prefix),
                lib.dirname,
                "lib" + get_lib_name(lib) + ".dylib",
            )
            dylib = hs.actions.declare_file(dylib_name)
            ln(hs, lib, dylib)
            new_libs.append(dylib)
        else:
            new_libs.append(lib)
    return new_libs

def windows_convert_to_dlls(hs, libs):
    """Convert .so dynamic libraries to .dll.

    Bazel's cc_library rule will create .so files for dynamic libraries even
    on Windows. GHC's builtin linker, which is used during compilation, GHCi,
    or doctests, hard-codes the assumption that all dynamic libraries on Windows
    end on .dll. This function serves as an adaptor and produces symlinks
    from a .dll version to the .so version for every dynamic library
    dependencies that does not end on .dll.

    Args:
      hs: Haskell context.
      libs: List of library files dynamic or static.

    Returns:
      List of library files where all dynamic libraries end on .dll.
    """
    lib_prefix = "_dlls"
    new_libs = []
    for lib in libs:
        if is_shared_library(lib) and lib.extension != "dll":
            dll_name = paths.join(
                target_unique_name(hs, lib_prefix),
                paths.dirname(lib.short_path),
                "lib" + get_lib_name(lib) + ".dll",
            )
            dll = hs.actions.declare_file(dll_name)
            ln(hs, lib, dll)
            new_libs.append(dll)
        else:
            new_libs.append(lib)
    return new_libs

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

def link_libraries(libs_to_link, args):
    """Add linker flags to link against the given libraries.

    Args:
      libs_to_link: List of library Files.
      args: Append arguments to this list.

    Returns:
      List of library names that were linked.

    """
    seen_libs = set.empty()
    libraries = []
    for lib in libs_to_link:
        lib_name = get_lib_name(lib)
        if not set.is_member(seen_libs, lib_name):
            set.mutable_insert(seen_libs, lib_name)
            args += ["-l{0}".format(lib_name)]
            libraries.append(lib_name)

def is_shared_library(f):
    """Check if the given File is a shared library.

    Args:
      f: The File to check.

    Returns:
      Bool: True if the given file `f` is a shared library, False otherwise.
    """
    return f.extension in ["so", "dylib"] or f.basename.find(".so.") != -1

def is_static_library(f):
    """Check if the given File is a static library.

    Args:
      f: The File to check.

    Returns:
      Bool: True if the given file `f` is a static library, False otherwise.
    """
    return f.extension in ["a"]

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

def link_forest(ctx, srcs, basePath = ".", **kwargs):
    """Write a symlink to each file in `srcs` into a destination directory
    defined using the same arguments as `ctx.actions.declare_directory`"""
    local_files = []
    for src in srcs.to_list():
        dest = ctx.actions.declare_file(
            paths.join(basePath, src.basename),
            **kwargs
        )
        local_files.append(dest)
        ln(ctx, src, dest)
    return local_files

def copy_all(ctx, srcs, dest):
    """Copy all the files in `srcs` into `dest`"""
    if list(srcs.to_list()) == []:
        ctx.actions.run_shell(
            command = "mkdir -p {dest}".format(dest = dest.path),
            outputs = [dest],
        )
    else:
        args = ctx.actions.args()
        args.add_all(srcs)
        ctx.actions.run_shell(
            inputs = depset(srcs),
            outputs = [dest],
            mnemonic = "Copy",
            command = "mkdir -p {dest} && cp -L -R \"$@\" {dest}".format(dest = dest.path),
            arguments = [args],
        )

def parse_pattern(ctx, pattern_str):
    """Parses a string label pattern.

    Args:
      ctx: Standard Bazel Rule context.

      pattern_str: The pattern to parse.
        Patterns are absolute labels in the local workspace. E.g.
        `//some/package:some_target`. The following wild-cards are allowed:
        `...`, `:all`, and `:*`. Also the `//some/package` shortcut is allowed.

    Returns:
      A struct of
        package: A list of package path components. May end on the wildcard `...`.
        target: The target name. None if the package ends on `...`. May be one
          of the wildcards `all` or `*`.

    NOTE: it would be better if Bazel itself exposed this functionality to Starlark.

    Any feature using this function should be marked as experimental, until the
    resolution of https://github.com/bazelbuild/bazel/issues/7763.
    """

    # We only load targets in the local workspace anyway. So, it's never
    # necessary to specify a workspace. Therefore, we don't allow it.
    if pattern_str.startswith("@"):
        fail("Invalid haskell_repl pattern. Patterns may not specify a workspace. They only apply to the current workspace")

    # To keep things simple, all patterns have to be absolute.
    if not pattern_str.startswith("//"):
        if not pattern_str.startswith(":"):
            fail("Invalid haskell_repl pattern. Patterns must start with either '//' or ':'.")

        # if the pattern string doesn't start with a package (it starts with :, e.g. :two),
        # then we prepend the contextual package
        pattern_str = "//{package}{target}".format(package = ctx.label.package, target = pattern_str)

    # Separate package and target (if present).
    package_target = pattern_str[2:].split(":", maxsplit = 2)
    package_str = package_target[0]
    target_str = None
    if len(package_target) == 2:
        target_str = package_target[1]

    # Parse package pattern.
    package = []
    dotdotdot = False  # ... has to be last component in the pattern.
    for s in package_str.split("/"):
        if dotdotdot:
            fail("Invalid haskell_repl pattern. ... has to appear at the end.")
        if s == "...":
            dotdotdot = True
        package.append(s)

    # Parse target pattern.
    if dotdotdot:
        if target_str != None:
            fail("Invalid haskell_repl pattern. ... has to appear at the end.")
    elif target_str == None:
        if len(package) > 0 and package[-1] != "":
            target_str = package[-1]
        else:
            fail("Invalid haskell_repl pattern. The empty string is not a valid target.")

    return struct(
        package = package,
        target = target_str,
    )

def match_label(patterns, label):
    """Whether the given local workspace label matches any of the patterns.

    Args:
      patterns: A list of parsed patterns to match the label against.
        Apply `parse_pattern` before passing patterns into this function.
      label: Match this label against the patterns.

    Returns:
      A boolean. True if the label is in the local workspace and matches any of
      the given patterns. False otherwise.

    NOTE: it would be better if Bazel itself exposed this functionality to Starlark.

    Any feature using this function should be marked as experimental, until the
    resolution of https://github.com/bazelbuild/bazel/issues/7763.
    """

    # Only local workspace labels can match.
    # Despite the docs saying otherwise, labels don't have a workspace_name
    # attribute. So, we use the workspace_root. If it's empty, the target is in
    # the local workspace. Otherwise, it's an external target.
    if label.workspace_root != "":
        return False

    package = label.package.split("/")
    target = label.name

    # Match package components.
    for i in range(min(len(patterns.package), len(package))):
        if patterns.package[i] == "...":
            return True
        elif patterns.package[i] != package[i]:
            return False

    # If no wild-card or mismatch was encountered, the lengths must match.
    # Otherwise, the label's package is not covered.
    if len(patterns.package) != len(package):
        return False

    # Match target.
    if patterns.target == "all" or patterns.target == "*":
        return True
    else:
        return patterns.target == target
