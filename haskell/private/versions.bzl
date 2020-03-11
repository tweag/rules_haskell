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

def check_version(actual_version):
    if type(actual_version) != "string" or len(actual_version) < 5:
        return  # Unexpected format

    expected_version = "2.0.0"  # Change THIS LINE when changing bazel version

    actual_major = int(actual_version[0])
    actual_minor = int(actual_version[2])
    actual_patch = int(actual_version[4])
    actual = (actual_major, actual_minor, actual_patch)

    expected_major = int(expected_version[0])
    expected_minor = int(expected_version[2])
    expected_patch = int(expected_version[4])
    expected = (expected_major, expected_minor, expected_patch)

    if actual < expected:
        print("WARNING: bazel version is too old. Expected {}, but found: {}".format(expected_version, actual_version))
        return
    if expected < actual:
        print("WARNING: bazel version is too recent. Expected {}, but found: {}".format(expected_version, actual_version))
        return
