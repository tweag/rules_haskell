"""Package identifiers"""

load("@bazel_skylib//lib:paths.bzl", "paths")
load(":private/mode.bzl", "is_profiling_enabled")

def _zencode(s):
    """Z-escape special characters to make a valid GHC package identifier.

    Args:
      s: string
    """
    return s.replace("Z", "ZZ").replace("_", "ZU").replace("/", "ZS").replace("~", "z7eU")

def _to_string(my_pkg_id):
    """Get a globally unique package identifier.

    The identifier is required to be unique for each Haskell rule.
    It includes the Bazel package and the name of this component.
    We can't use just the latter because then two components with
    the same names in different packages would clash.
    """
    return _zencode(
        paths.join(
            my_pkg_id.label.workspace_root,
            my_pkg_id.label.package,
            my_pkg_id.name,
        ),
    )

def _new(label, package_name = None, version = None):
    """Create a new package identifier.

    Package identifiers should be globally unique. This is why we use
    a label to identify them.

    Args:
      label: The label of the rule declaring the package.
      package_name: an optional override of the package name.
      version: an optional version annotation.

    Returns:
      string: GHC package ID to use.

    """
    name = label.name.replace("_", "-")
    return struct(
        label = label,
        name = name,
        package_name = package_name if package_name else name,
        version = version,
    )

def _library_name(hs, my_pkg_id, prof_suffix = False):
    """Get library name.

    Args:
      hs: Haskell context.
      my_pkg_id: pkg_id struct.
      prof_suffix: whether to automatically add profiling suffix.
    """
    library_name = "HS" + _to_string(my_pkg_id)
    if is_profiling_enabled(hs) and prof_suffix:
        library_name += "_p"
    return library_name

pkg_id = struct(
    new = _new,
    to_string = _to_string,
    library_name = _library_name,
)
