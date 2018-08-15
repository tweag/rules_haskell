"""Package identifiers"""

load("@bazel_skylib//:lib.bzl", "paths")
load(":private/mode.bzl", "is_profiling_enabled")

def _zencode(s):
    """Z-escape special characters to make a valid GHC package identifier.

    Args:
      s: string
    """
    return s.replace("Z", "ZZ").replace("_", "ZU").replace("/", "ZS")

def _to_string(label):
    """Get a globally unique package identifier.

    The identifier is required to be unique for each Haskell rule.
    It includes the Bazel package and the name of this component.
    We can't use just the latter because then two components with
    the same names in different packages would clash.
    """
    return _zencode(
        paths.join(
            label.workspace_root,
            label.package,
            label.name,
        ),
    )

def _new(label, version = None):
    """Create a new package identifier.

    Package identifiers should be globally unique. This is why we use
    a label to identify them.

    Args:
      label: The label of the rule declaring the package.
      version: an optional version annotation.

    Returns:
      string: GHC package ID to use.

    """
    return struct(
        label = label,
        name = _to_string(label),
        version = version,
    )

def _library_name(hs, my_pkg_id, prof_suffix = False):
    """Get library name.

    Args:
      hs: Haskell context.
      my_pkg_id: pkg_id struct.
      prof_suffix: whether to automatically add profiling suffix.
    """
    library_name = "HS" + my_pkg_id.name
    if is_profiling_enabled(hs) and prof_suffix:
        library_name += "_p"
    return library_name

pkg_id = struct(
    new = _new,
    library_name = _library_name,
)
