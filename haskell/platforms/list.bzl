load("//haskell:private/dict.bzl", "find")

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
                visibility = ["//visibility:public"],
            )
    for arch, constraint_value in ARCH.items():
        if constraint_value:
            native.config_setting(
                name = arch,
                constraint_values = [constraint_value],
                visibility = ["//visibility:public"],
            )

def os_of_constraints(constraints):
    """ Returns the os corresponding to the first os constraint.
    If there are none, returns None.
    """
    for c in constraints:
        if c.package == "os":
            return find(OS, str(c))

def arch_of_constraints(constraints):
    """ Returns the architecture corresponding to the first arch constraint.
    If there are none, returns None.
    """
    for c in constraints:
        if c.package == "cpu":
            return find(ARCH, str(c))

def platform_of_constraints(constraints):
    os = os_of_constraints(constraints)
    if os == None:
        fail("Could not find os in constraints {}".format(constraints))

    arch = arch_of_constraints(constraints)
    if arch == None:
        fail("Could not find arch in constraints {}".format(constraints))

    return ("{}_{}".format(os, arch))
