# check_bazel_version_compatible below cannot be called from anywhere
# because native.bazel_version's availability is restricted:
#   https://github.com/bazelbuild/bazel/issues/8305
# An alternative hacky way is as follows:
#   https://github.com/protocolbuffers/upb/blob/master/bazel/repository_defs.bzl
#
# There are utility functions for parsing versions numbers here:
#   load("@bazel_skylib//lib:versions.bzl", "versions")
# But we don't want to use them, as skylib is not yet loaded when code
# in this file executes (and there's no way to execute it later; see first
# paragraph above).

# It's important to keep this list short enough (not more then 4 items)
# because every bazel version tested requires a lot of space on CI
# See https://github.com/tweag/rules_haskell/pull/1781#issuecomment-1187640454
SUPPORTED_BAZEL_VERSIONS = [
    "6.0.0",
    "6.3.2",
    "6.4.0",
]

SUPPORTED_NIXPKGS_BAZEL_PACKAGES = [
    "bazel_6",
]

def _parse_version_chunk(version_chunk):
    """
    Args:
      version_chunk: a chunk of a semantic version, possibly with trailing
                     content at the end; such as "29", "2", or "3 foobar".
                     We handle trailing content because
                     native.bazel_version can: try printing actual_version
                     in check_version below (got for example
                     "2.0.0- (@non-git)").
    Returns:
      The chunk string with trailing content removed, such as "29", "2", or "3"
    """
    for i in range(len(version_chunk)):
        c = version_chunk[i]
        if not c.isdigit():
            return version_chunk[:i]
    return version_chunk

def _parse_bazel_version(bazel_version):
    """
    Args:
      bazel_version: a string that starts with a semantic version
                     like "2.0" or "0.29.1" or "0.29 foobar"
    Returns:
      The version as a int list such as [2, 0], [0, 29, 1], or [0, 29]
    """
    return [int(_parse_version_chunk(x)) for x in bazel_version.split(".")]

def is_at_least(threshold, version):
    """Check that a version is higher or equals to a threshold.
    Args:
      threshold: the minimum version string
      version: the version string to be compared to the threshold
    Returns:
      True if version >= threshold.
    """

    # Vendored from https://github.com/bazelbuild/bazel-skylib/blob/e30197f3799eb038fbed424e365573f493d52fa5/lib/versions.bzl
    # Needed for check_bazel_version below.
    return _parse_bazel_version(version) >= _parse_bazel_version(threshold)

def is_at_most(threshold, version):
    """Check that a version is lower or equals to a threshold.
    Args:
      threshold: the maximum version string
      version: the version string to be compared to the threshold
    Returns:
      True if version <= threshold.
    """

    # Vendored from https://github.com/bazelbuild/bazel-skylib/blob/e30197f3799eb038fbed424e365573f493d52fa5/lib/versions.bzl
    # Needed for check_bazel_version below.
    return _parse_bazel_version(version) <= _parse_bazel_version(threshold)

def check_bazel_version(minimum_bazel_version, maximum_bazel_version = None, bazel_version = None):
    """Check that the version of Bazel is valid within the specified range.
    Args:
      minimum_bazel_version: minimum version of Bazel expected
      maximum_bazel_version: maximum version of Bazel expected
      bazel_version: the version of Bazel to check. Used for testing, defaults to native.bazel_version
    Returns:
      (bool, string):
        bool: `True`, if the version meets the criteria, otherwise `False`.
        string: An appropriate message if the version doesn't match.
    """

    # Vendored from https://github.com/bazelbuild/bazel-skylib/blob/e30197f3799eb038fbed424e365573f493d52fa5/lib/versions.bzl#L82
    # The upstream version `fail`s if the version doesn't match.
    # This version instead returns false.
    if not bazel_version:
        if "bazel_version" not in dir(native):
            return (False, "Current Bazel version is lower than 0.2.1; expected at least {}".format(
                minimum_bazel_version,
            ))
        elif not native.bazel_version:
            # Using a non-release version, assume it is good.
            return (True, "")
        else:
            bazel_version = native.bazel_version

    if not is_at_least(
        threshold = minimum_bazel_version,
        version = bazel_version,
    ):
        return (False, "Current Bazel version is {}; expected at least {}".format(
            bazel_version,
            minimum_bazel_version,
        ))

    if maximum_bazel_version:
        if not is_at_most(
            threshold = maximum_bazel_version,
            version = bazel_version,
        ):
            return (False, "Current Bazel version is {}; expected at most {}".format(
                bazel_version,
                maximum_bazel_version,
            ))

    return (True, "")

def check_bazel_version_compatible(actual_version):
    min_bazel = SUPPORTED_BAZEL_VERSIONS[0]
    max_bazel = SUPPORTED_BAZEL_VERSIONS[-1]

    (compatible, msg) = check_bazel_version(min_bazel, max_bazel, actual_version)

    if not compatible:
        print("WARNING:", msg)
