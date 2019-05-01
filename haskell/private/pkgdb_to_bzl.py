#!/usr/bin/env python
#
# Convert a package database to a .bzl file loadable from a BUILD
# description.

from __future__ import unicode_literals

import glob
import os
import sys
import textwrap
import types

if len(sys.argv) == 3:
    repo_dir = "external/" + sys.argv[1]
    topdir = sys.argv[2]
else:
    sys.exit("Usage: pkgdb_to_bzl.py <REPO_NAME> <TOPDIR>")

def unfold_fields(content):
    """Unfold fields that were split over multiple lines.

    Returns:
        A list of field name/value pairs.
    """
    fields = []
    for line in content.splitlines():
        if line.startswith(" "):
            fields[-1] += line
        elif line:
            fields.append(line)
    return fields

def path_to_label(path, pkgroot):
    """Substitute one pkgroot for another relative one to obtain a label."""
    topdir_relative_path = path.replace(pkgroot, "$topdir")
    if topdir_relative_path.find("$topdir") != -1:
        return topdir_relative_path.replace("$topdir", topdir).replace('\\', '/')

pkg_confs = {}
for conf in glob.glob(os.path.join(topdir, "package.conf.d", "*.conf")):
    with open(conf, 'r') as f:
        pkg_confs[conf] = f.read()

output = []

# Accumulate package id to package name mappings.
pkg_id_map = []
for conf, content in pkg_confs.items():
    fields = unfold_fields(content)
    pkg = types.SimpleNamespace(
        include_dirs = [],
        library_dirs = [],
        dynamic_library_dirs = [],
        depends = [],
        ld_options = [],
        extra_libraries = [],
    )
    for field in fields:
        key, value = field.split(":", 1)
        value = value.strip()
        if key == "name":
            pkg.name = value
        elif key == "version":
            pkg.version = value
        elif key == "id":
            pkg.id = value
        elif key == "include-dirs":
            pkg.include_dirs = value.split()
        elif key == "library-dirs":
            pkg.library_dirs = value.split()
        elif key == "dynamic-library-dirs":
            pkg.dynamic_library_dirs = value.split()
        elif key == "hs-libraries":
            pkg.hs_libraries = value.split()
        elif key == "depends":
            pkg.depends = value.split()
        elif key == "ld-options":
            pkg.ld_options = [opt.strip('"') for opt in value.split()]
        elif key == "extra-libraries":
            pkg.extra_libraries = value.split()

    # pkgroot is not part of .conf files. It's a computed value. It is
    # defined to be the directory enclosing the package database
    # directory.
    pkg.pkgroot = os.path.dirname(os.path.dirname(os.path.realpath(conf)))

    pkg_id_map.append((pkg.name, pkg.id))
    for hs_library in pkg.hs_libraries:
        output += [
            textwrap.dedent("""\
                haskell_import(
                    name = "{name}",
                    id = "{id}",
                    deps = {deps},
                    hdrs = {hdrs},
                    includes = {includes},
                    linkopts = {linkopts},
                    shared_library = {shared_library}[0] if {shared_library} else None,
                    static_library = {static_library}[0] if {static_library} else None,
                    static_profiling_library = {static_profiling_library}[0] if {static_profiling_library} else None,
                    version = "{version}",
                    visibility = ["//visibility:public"],
                )
                """.format(
                    name = pkg.name,
                    id = pkg.id,
                    version = pkg.version,
                    hdrs = "glob({})".format([
                        path_to_label("{}/**/*.h".format(include_dir), pkg.pkgroot)
                        for include_dir in pkg.include_dirs
                        if path_to_label(include_dir, pkg.pkgroot)
                    ]),
                    includes = [
                        "/".join([repo_dir, path_to_label(include_dir, pkg.pkgroot)])
                        for include_dir in pkg.include_dirs
                        if path_to_label(include_dir, pkg.pkgroot)
                    ],
                    static_library = "glob({})".format([
                        path_to_label("{}/lib{}.a".format(library_dir, hs_library), pkg.pkgroot)
                        for library_dir in pkg.library_dirs
                        if path_to_label(library_dir, pkg.pkgroot)
                    ]),
                    static_profiling_library = "glob({})".format([
                        path_to_label("{}/lib{}_p.a".format(library_dir, hs_library), pkg.pkgroot)
                        for library_dir in pkg.library_dirs
                        if path_to_label(library_dir, pkg.pkgroot)
                    ]),
                    shared_library = "glob({})".format([
                        path_to_label(
                            "{}/lib{}-ghc*.{}".format(
                                dynamic_library_dir,
                                hs_library,
                                ext,
                            ),
                            pkg.pkgroot,
                        )
                        for dynamic_library_dir in pkg.dynamic_library_dirs + pkg.library_dirs
                        if path_to_label(dynamic_library_dir, pkg.pkgroot)
                        for ext in ["dll", "dylib", "so"]
                    ]),
                    deps = pkg.depends,
                    linkopts = pkg.ld_options + [
                        "-L{}".format(library_dir)
                        for library_dir in pkg.dynamic_library_dirs + pkg.library_dirs
                        if not path_to_label(library_dir, pkg.pkgroot)
                    ] + [
                        "-l{}".format(extra_library)
                        for extra_library in pkg.extra_libraries
                    ],
                )
            )
        ]

for pkg_name, pkg_id in pkg_id_map:
    if pkg_id != pkg_name:
        output += ["""alias(name = '{}', actual = '{}')""".format(pkg_id, pkg_name)]

output += [
    textwrap.dedent("""
      toolchain_libraries = {pkgs}
      """.format(pkgs = [pkg_name for pkg_name, _ in pkg_id_map])
    )
]

print("\n".join(output))
