OS = {
    "aix": None,
    "darwin": "@platforms//os:osx",
    "dragonfly": None,
    "freebsd": "@platforms//os:freebsd",
    "haiku": None,
    "hpux": None,
    "ios": "@platforms//os:ios",
    "linux_android": "@platforms//os:android",
    "linux": "@platforms//os:linux",
    "mingw32": "@platforms//os:windows",
    "netbsd": None,
    "openbsd": None,
    "solaris2": None,
}

ARCH = {
    "aarch64": None,
    "alpha": None,
    "arm64": "@platforms//cpu:aarch64",
    "arm": "@platforms//cpu:arm",
    "i386": "@platforms//cpu:x86_32",
    "ia64": None,
    "powerpc64": None,
    "powerpc64le": None,
    "powerpc": "@platforms//cpu:ppc",
    "rs6000": None,
    "sparc": None,
    "x86_64": "@platforms//cpu:x86_64",
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
