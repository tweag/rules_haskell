# check_version below cannot be called from anywhere
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

def check_version(actual_version):
    if type(actual_version) != "string" or len(actual_version) < 5:
        return  # Unexpected format

    # Please use length 3 tuples, because bazel versions has 3 members;
    # to avoid surprising behaviors (for example (2,0) >/= (2, 0, 0))
    min_bazel = (2, 1, 0)  # Change THIS LINE when changing bazel min version
    max_bazel = (2, 1, 0)  # Change THIS LINE when changing bazel max version

    actual = tuple(_parse_bazel_version(actual_version))

    if (min_bazel <= actual) and (actual <= max_bazel):
        return  # All good

    min_bazel_string = ".".join([str(x) for x in min_bazel])
    max_bazel_string = ".".join([str(x) for x in max_bazel])

    adjective = "old" if actual < min_bazel else "recent"

    print("WARNING: bazel version is too {}. Supported versions range from {} to {}, but found: {}".format(adjective, min_bazel_string, max_bazel_string, actual_version))
