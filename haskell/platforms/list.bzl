OS = {
    "aix": None,
    "darwin": "@bazel_tools//platforms:osx",
    "dragonfly": None,
    "freebsd": "@bazel_tools//platforms:freebsd",
    "haiku": None,
    "hpux": None,
    "ios": "@bazel_tools//platforms:ios",
    "linux_android": "@bazel_tools//platforms:android",
    "linux": "@bazel_tools//platforms:linux",
    "mingw32": "@bazel_tools//platforms:windows",
    "netbsd": None,
    "openbsd": None,
    "solaris2": None,
}

ARCH = {
    "aarch64": None,
    "alpha": None,
    "arm64": "@bazel_tools//platforms:aarch64",
    "arm": "@bazel_tools//platforms:arm",
    "i386": "@bazel_tools//platforms:x86_32",
    "ia64": None,
    "powerpc64": None,
    "powerpc64le": None,
    "powerpc": "@bazel_tools//platforms:ppc",
    "rs6000": None,
    "sparc": None,
    "x86_64": "@bazel_tools//platforms:x86_64",
}

def declare_config_settings():
    for os, constraint_value in OS.items():
        if constraint_value:
            native.config_setting(
                name = os,
                constraint_values = [constraint_value],
            )
    for arch, constraint_value in ARCH.items():
        if constraint_value:
            native.config_setting(
                name = arch,
                constraint_values = [constraint_value],
            )
