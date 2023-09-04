#!/usr/bin/env python3
"""Generate Cabal version macros.

Generates the content of a C header file for the given library name and version
and prints it to standard output.
"""

import argparse


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("name", help="The package name.")
    parser.add_argument("version", help="The package version.")
    args = parser.parse_args()

    print(version_macros(args.name, args.version))


def version_macros(name, version):
    """Generate Cabal version macros.

    Based on Cabal's version macro generation, see [1].

    [1]: http://hackage.haskell.org/package/Cabal-2.4.1.0/docs/src/Distribution.Simple.Build.Macros.html#generatePackageVersionMacros
    """
    (major1, major2, minor) = version_components(version)
    escaped_name = cpp_escape_name(name)
    return "\n".join([
        # #define VERSION_pkg "1.2.3"
        cpp_ifndef_define(
            "VERSION_" + escaped_name,
            [],
            '"{}"'.format(version),
        ),
        # #define MIN_VERSION_pkg(major1, major2, minor) ...
        cpp_ifndef_define(
            "MIN_VERSION_" + escaped_name,
            ["major1", "major2", "minor"],
            " \\\n".join([
                "(",
                "  (major1) < {} ||".format(major1),
                "  (major1) == {} && (major2) < {} ||".format(major1, major2),
                "  (major1) == {} && (major2) == {} && (minor) <= {} )".format(
                    major1, major2, minor),
            ])),
    ])


def version_components(version):
    """Split version string into major1.major2.minor components."""
    components = version.split(".")
    num = len(components)

    if num < 1:
        raise ValueError("version should have at least one component.")

    major1 = components[0]

    if num >= 2:
        major2 = components[1]
    else:
        major2 = "0"

    if num >= 3:
        minor = components[2]
    else:
        minor = "0"

    return (major1, major2, minor)


def cpp_escape_name(name):
    """Escape package name to be CPP macro safe."""
    return name.replace("-", "_")


def cpp_define(macro, params, val):
    """CPP macro definition, optionally with parameters."""
    return "#define {macro}{params} {val}".format(
        macro = macro,
        params = "({})".format(",".join(params)) if params else "",
        val = val,
    )


def cpp_ifndef(macro, body):
    """CPP ifndef block."""
    return "#ifndef {macro}\n{body}\n#endif /* {macro} */".format(
        macro = macro,
        body = body,
    )


def cpp_ifndef_define(macro, params, val):
    """CPP macro definition, if not previously defined."""
    return cpp_ifndef(macro, cpp_define(macro, params, val))


if __name__ == "__main__":
    main()
