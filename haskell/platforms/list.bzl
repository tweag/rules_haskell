load("//haskell:private/dict.bzl", "find")

OS = {
    "aix": None,
    "darwin": "osx",
    "dragonfly": None,
    "freebsd": "freebsd",
    "haiku": None,
    "hpux": None,
    "ios": "ios",
    "linux_android": "android",
    "linux": "linux",
    "mingw32": "windows",
    "netbsd": None,
    "openbsd": None,
    "solaris2": None,
}

ARCH = {
    "aarch64": None,
    "alpha": None,
    "arm64": "aarch64",
    "arm": "arm",
    "i386": "x86_32",
    "ia64": None,
    "powerpc64": None,
    "powerpc64le": None,
    "powerpc": "ppc",
    "rs6000": None,
    "sparc": None,
    "x86_64": "x86_64",
}

def declare_config_settings():
    for os, constraint_value in OS.items():
        if constraint_value:
            native.config_setting(
                name = os,
                constraint_values = ["@platforms//os:{}".format(constraint_value)],
                visibility = ["//visibility:public"],
            )
    for arch, constraint_value in ARCH.items():
        if constraint_value:
            native.config_setting(
                name = arch,
                constraint_values = ["@platforms//cpu:{}".format(constraint_value)],
                visibility = ["//visibility:public"],
            )

def os_of_constraints(constraints):
    """ Returns the os corresponding to the first os constraint.
    If there are none, returns None.
    """
    for c in constraints:
        if c.package == "os":
            return find(OS, c.name)
    return None

def arch_of_constraints(constraints):
    """ Returns the architecture corresponding to the first arch constraint.
    If there are none, returns None.
    """
    for c in constraints:
        if c.package == "cpu":
            return find(ARCH, c.name)
    return None

def platform_of_constraints(constraints):
    os = os_of_constraints(constraints)
    if os == None:
        fail("Could not find os in constraints {}".format(constraints))

    arch = arch_of_constraints(constraints)
    if arch == None:
        fail("Could not find arch in constraints {}".format(constraints))

    return ("{}_{}".format(os, arch))
