"""Utilities for module and path manipulations."""

load("@bazel_skylib//lib:paths.bzl", "paths")
load("@bazel_skylib//lib:sets.bzl", "sets")

def is_haskell_extension(extension):
    """Whether the given extension defines a Haskell source file."""
    return extension in ["hs", "hs-boot", "hsc", "lhs", "lhs-boot"]

def is_valid_module_component(component):
    """Whether the given string is a valid Haskell module name component.

    Based on the logic in `Cabal.Distribution.ModuleName.validModuleComponent`.
    """
    if len(component) == 0:
        # Must not be empty
        return False
    if not (component[0].isalpha() and component[0].isupper()):
        # Must start with an upper case alphabetic character
        return False
    for char in component.elems():
        # Must consist of alphanumeric characters or _ or '
        if not (char.isalnum() or char == "_" or char == "'"):
            return False
    return True

def longest_valid_module_name(path):
    """Determine the expected module name from a source file path.

    Takes the longest suffix of valid module name components. E.g.

      src/Hierarchical/Module.hs --> Hierarchical.Module
      src/Prefix/invalid/Valid/Module.hs --> Valid.Module

    Returns the empty string if no valid module name component was found.
    """
    components = paths.split_extension(path)[0].split("/")
    cutoff = 0
    for i in range(len(components), 0, -1):
        if not is_valid_module_component(components[i - 1]):
            cutoff = i
            break
    return ".".join(components[cutoff:len(components)])

def infer_main_module(main_function):
    """Infer the name of the module containing the main function.

    This defaults to `Main` unless `main_function` specifies a different
    module name.
    """
    components = main_function.split(".")
    for i in range(len(components)):
        if not is_valid_module_component(components[i]):
            break
    if i == 0:
        return "Main"
    else:
        return ".".join(components[0:i])

def _module_map_insert(module_map, module_name, module_file, is_boot = False):
    entry = module_map.get(module_name, struct(src = None, boot = None))
    if is_boot:
        if entry.boot:
            fail("Found two boot files for module %s: %s and %s" % (module_name, entry.boot, module_file))
        module_map[module_name] = struct(
            boot = module_file,
            src = entry.src,
        )
    else:
        if entry.src:
            fail("Found two source files for module %s: %s and %s" % (module_name, entry.src, module_file))
        module_map[module_name] = struct(
            boot = entry.boot,
            src = module_file,
        )

def determine_module_names(src_files, search_for_main = False, main_function = "", main_file = None):
    """Determine a mapping from module names to source files.

    The module name is inferred from the source file name. See
    `longest_valid_module_name` for details.

    If the target is a binary then a main module is required. The main module's
    name is `Main` unless `main_function` specifies another module name. If
    `main_file` is specified then it must use that main module name. If
    `main_file` is not specified then we use the following heuristics to
    determine the main module:
    * A source file's `longest_valid_module_name` is the main module name.
      E.g. `Main.hs`.
    * One source file's path does not yield a valid module name.
      E.g. `exe.hs`.
    * The target has only a single Haskell source file.
      E.g. `App.hs`.

    Args:
      src_files: sequence of File, source files.
      search_for_main: bool, whether we need to ensure there's a main module.
      main_function: string, optional, the `main_function` attribute to a Haskell binary rule.
      main_file: File, optional, the `main_file` attribute to a Haskell binary rule.

    Returns:
      dict(module_name: module_info):
        module_name: string, the Haskell module name.
        module_info: struct(src, boot):
          src: File, the module source file.
          boot: File, optional, the boot file for cyclic module dependencies.
    """
    module_map = {}
    undetermined = []

    for src in src_files:
        if not is_haskell_extension(src.extension) or src == main_file:
            continue
        module_name = longest_valid_module_name(src.short_path)
        if not module_name:
            undetermined.append(src)
        else:
            _module_map_insert(module_map, module_name, src, is_boot = src.short_path.endswith("-boot"))

    if search_for_main:
        main_module = infer_main_module(main_function)
        if main_file:
            if main_file in src_files:
                _module_map_insert(module_map, main_module, main_file)
            else:
                fail("""\
main_file = "{}" is not listed in the "srcs" attribute.
""".format(main_file.path))

        elif not main_module in module_map:
            if len(undetermined) == 1:
                _module_map_insert(module_map, main_module, undetermined.pop())
            elif len(module_map) == 1:
                (_, entry) = module_map.popitem()
                module_map[main_module] = entry
            else:
                fail("""\
No source file defining the main module '{main_module}'.
You may need to set the 'main_file' attribute or the 'module_name' attribute in a 'haskell_module' rule.
""".format(main_module = main_module))

    if undetermined:
        fail("""\
Could not determine module names for:
{undetermined}
The file name must match the module name.
E.g. `src/My/Module.hs` for `My.Module`.
""".format(undetermined = "\n".join(["  %s" % src for src in undetermined])))

    boot_only = [entry.boot for entry in module_map.values() if not entry.src]
    if boot_only:
        fail("""\
Found boot file without matching source file:
{boot_files}
""".format(boot_files = "\n".join(["  %s" % boot for boot in boot_only])))

    return module_map

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

def join_path_list(is_windows, paths):
    """Join the given paths suitable for env vars like PATH.

    Joins the list of given paths into a single string separated by ':' on Unix
    or ';' on Windows.
    """
    sep = ";" if is_windows else ":"
    return sep.join(paths)

def mangle_static_library(hs, posix, dynamic_lib, static_lib, outdir):
    """Mangle a static C library to match a dynamic C library name.

    GHC expects static and dynamic C libraries to have matching library names.
    Bazel produces static and dynamic C libraries with different names. The
    dynamic library names are mangled, the static library names are not.

    If the dynamic library exists and if the static library exists and has a
    different name. Then this function will create a symbolic link for the
    static library to match the dynamic library's name.

    Args:
      hs: Haskell context.
      dynamic_lib: File or None, the dynamic library.
      static_lib: File or None, the static library.
      outdir: Output director for the symbolic link, if necessary.

    Returns:
      The new static library symlink, if created, otherwise None.
    """
    if dynamic_lib == None or static_lib == None:
        return None
    libname = get_lib_name(dynamic_lib)
    if get_lib_name(static_lib) == libname:
        return None
    else:
        link = hs.actions.declare_file(
            paths.join(outdir, "lib" + libname + "." + static_lib.extension),
        )
        ln(hs, posix, static_lib, link)
        return link

def get_dirname(file):
    """Return the path to the directory containing the file."""
    return file.dirname

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

def get_lib_extension(lib):
    """Return extension of the library

    Takes extensions such as so.1.2.3 into account.

    Args:
      lib: The library File.

    Returns:
      String: extension of the library.
    """
    n = lib.basename.find(".so.")
    end = lib.extension if n == -1 else lib.basename[n + 1:]
    return end

def get_dynamic_hs_lib_name(ghc_version, lib):
    """Return name of library by dropping extension,
    "lib" prefix, and GHC version suffix.

    Args:
      version: GHC version.
      lib: The library File.

    Returns:
      String: name of library.
    """
    name = get_lib_name(lib)
    suffix = "-ghc{}".format(ghc_version)
    if name.endswith(suffix):
        name = name[:-len(suffix)]
    return name

def get_static_hs_lib_name(with_profiling, lib):
    """Return name of library by dropping extension,
    "lib" prefix, and potential profiling suffix.

    Args:
      with_profiling: Whether profiling mode is enabled.
      lib: The library File.

    Returns:
      String: name of library.
    """
    name = get_lib_name(lib)
    suffix = "_p" if with_profiling else ""
    if suffix and name.endswith(suffix):
        name = name[:-len(suffix)]
    if name == "Cffi":
        name = "ffi"
    return name

# tests in /tests/unit_tests/BUILD
def parent_dir_path(path):
    """Returns the path of the parent directory.
    For a relative path with just a file, "." is returned.
    The path is not normalized.

    foo => .
    foo/ => foo
    foo/bar => foo
    foo/bar/baz => foo/bar
    foo/../bar => foo/..

    Args:
      a path string

    Returns:
      A path list of the form `["foo", "bar"]`
    """
    path_dir = paths.dirname(path)

    # dirname returns "" if there is no parent directory
    # In that case we return the identity path, which is ".".
    if path_dir == "":
        return ["."]
    else:
        return path_dir.split("/")

def __check_dots(target, path):
    # there’s still (non-leading) .. in split
    if ".." in path:
        fail("the short_path of target {} (which is {}) contains more dots than loading `../`. We can’t handle that.".format(
            target,
            target.short_path,
        ))

# starlark doesn’t allow nested defs, which is a mystery.
def _get_target_parent_dir(target):
    """get the parent dir and handle leading short_path dots,
    which signify that the target is in an external repository.

    Args:
      target: a target, .short_path is used
    Returns:
      (is_external, parent_dir)
      `is_external`: Bool whether the path points to an external repository
      `parent_dir`: The parent directory, either up to the runfiles toplel,
                    up to the external repository toplevel.
                    Is `[]` if there is no parent dir.
    """

    parent_dir = parent_dir_path(target.short_path)

    if parent_dir[0] == "..":
        __check_dots(target, parent_dir[1:])
        return (True, parent_dir[1:])
    elif parent_dir[0] == ".":
        return (False, [])
    else:
        __check_dots(target, parent_dir)
        return (False, parent_dir)

def relative_rpath_prefix(is_darwin):
    """Returns the prefix for relative RUNPATH entries.

    Args:
      is_darwin: Whether the target platform is Darwin.

    Returns:
      string, `@loader_path` on Darwin, `$ORIGIN` otherwise.
    """
    if is_darwin:
        return "@loader_path"
    else:
        return "$ORIGIN"

# tests in /tests/unit_tests/BUILD
def create_rpath_entry(
        binary,
        dependency,
        keep_filename,
        prefix = ""):
    """Return a (relative) path that points from `binary` to `dependecy`
    while not leaving the current bazel runpath, taking into account weird
    corner cases of `.short_path` concerning external repositories.
    The resulting entry should be able to be inserted into rpath or similar.

    Examples:

      bin.short_path=foo/a.so and dep.short_path=bar/b.so
        => create_rpath_entry(bin, dep, False) = ../bar
           and
           create_rpath_entry(bin, dep, True) = ../bar/b.so
           and
           create_rpath_entry(bin, dep, True, "$ORIGIN") = $ORIGIN/../bar/b.so

    Args:
      binary: target of current binary
      dependency: target of dependency to relatively point to
      keep_filename: whether to point to the filename or its parent dir
      prefix: string path prefix to add before the relative path

    Returns:
      relative path string
    """

    (bin_is_external, bin_parent_dir) = _get_target_parent_dir(binary)
    (dep_is_external, dep_parent_dir) = _get_target_parent_dir(dependency)

    # backup through parent directories of the binary,
    # to the runfiles directory
    bin_backup = [".."] * len(bin_parent_dir)

    # external repositories live in `target.runfiles/external`,
    # while the internal repository lives in `target.runfiles`.
    # The `.short_path`s of external repositories are strange,
    # they start with `../`, but you cannot just append that in
    # order to find the correct runpath. Instead you have to use
    # the following logic to construct the correct runpaths:
    if bin_is_external:
        if dep_is_external:
            # stay in `external`
            path_segments = bin_backup
        else:
            # backup out of `external`
            path_segments = [".."] + bin_backup
    elif dep_is_external:
        # go into `external`
        path_segments = bin_backup + ["external"]
    else:
        # no special external traversal
        path_segments = bin_backup

    # then add the parent dir to our dependency
    path_segments.extend(dep_parent_dir)

    # optionally add the filename
    if keep_filename:
        path_segments.append(
            paths.basename(dependency.short_path),
        )

    # normalize for good measure and create the final path
    path = paths.normalize("/".join(path_segments))

    # and add the prefix if applicable
    if prefix == "":
        return path
    else:
        return prefix + "/" + path

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
def truly_relativize(target, relative_to):
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

def rel_to_pkgroot(target, pkgdb):
    """Construct path relative to the package root.

    Args:
      target: Target path.
      pkgdb: Path to the package db directory. E.g. cache_file.dirname.

    Returns:
      Path relative to package root, e.g. ${pkgroot}/relative/path.
    """
    return paths.join(
        "${pkgroot}",
        truly_relativize(target, paths.dirname(pkgdb)),
    )

def ln(hs, posix, target, link, extra_inputs = depset()):
    """Create a symlink to target.

    Args:
      hs: Haskell context.
      extra_inputs: extra phony dependencies of symlink.

    Returns:
      None
    """
    relative_target = truly_relativize(target.path, link.dirname)
    hs.actions.run_shell(
        inputs = depset([target], transitive = [extra_inputs]),
        outputs = [link],
        mnemonic = "Symlink",
        command = '"{ln}" -s {target} {link}'.format(
            ln = posix.commands["ln"],
            target = relative_target,
            link = link.path,
        ),
        # Don't sandbox symlinking to reduce overhead.
        # See https://github.com/tweag/rules_haskell/issues/958.
        execution_requirements = {
            "no-sandbox": "",
            "no-cache": "",
            "no-remote": "",
        },
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
    package_target = pattern_str[2:].split(":", 2)
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
