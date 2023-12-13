"""Module for managing/deriving official Bazel platform values."""

# The expected values for os_cpu can be found by looking at the
# implementation for get_cpu_value in lib_cc_configure.bzl.
# https://github.com/bazelbuild/bazel/blob/e11506feaea7401c3d27f55b47183ef49bd1d5a8/tools/cpp/lib_cc_configure.bzl#L186

# TODO(chuck): Add unit tests.

def _get_os(os_cpu):
    if os_cpu.find("darwin") >= 0:
        return "osx"
    if os_cpu.find("windows") >= 0:
        return "windows"
    return "linux"

def _get_cpu(os_cpu):
    # This value could appear in older versions of Bazel.
    if os_cpu == "darwin":
        return "x86_64"

    # This handles modern os-cpu values like darwin_arm64 or darwin_x86_64.
    if os_cpu.startswith("darwin_"):
        return os_cpu.removeprefix("darwin_")
    if os_cpu == "arm64_windows":
        return "arm64"
    if os_cpu == "x64_windows":
        return "x86_64"

    return "linux"

bazel_platforms = struct(
    get_os = _get_os,
    get_cpu = _get_cpu,
)
