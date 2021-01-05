#!/usr/bin/env python3
#
# Convert a package database to a .bzl file loadable from a BUILD
# description.
#
# Arguments:;
#     REPO_NAME: the name of the external repository in which the database is stored.
#     TOP_DIR: The path to GHC's $topdir, i.e. the lib/ folder of any installation.

from __future__ import unicode_literals

import glob
import os
import sys
import textwrap
import types

import package_configuration

if len(sys.argv) == 3:
    repo_dir = "external/" + sys.argv[1]
    topdir = sys.argv[2]
else:
    sys.exit("Usage: pkgdb_to_bzl.py <REPO_NAME> <TOPDIR>")

def unfold_fields(content):
    """Unfold fields that were split over multiple lines.

    Returns:
        A list of strings. Each string represents one field (a name/value pair
        separated by a colon).

    >>> unfold_fields("foo  \n   bar  \n   baz  \nbiz   \nboz   ")
    ['foo     bar     baz  ', 'biz   ', 'boz   ']
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
        return os.path.normpath(topdir_relative_path.replace("$topdir", topdir)).replace('\\', '/')

def hs_library_pattern(name, mode = "static", profiling = False):
    """Convert hs-libraries entry to glob patterns.

    Args:
        name: The library name. E.g. HSrts or Cffi.
        mode: The linking mode. Either "static" or "dynamic".
        profiling: Look for profiling mode libraries.

    Returns:
        List of globbing patterns for the library file.

    """
    # The RTS configuration suffix.
    # See https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/config#rts-configurations
    configs = ["_p"] if profiling else [""]
    # Special case HSrts or Cffi - include both libXYZ and libXYZ_thr.
    if name == "HSrts" or name == "Cffi":
        configs = [
            prefix + config
            for config in configs
            for prefix in ["", "_thr"]
        ]
    # Special case libCffi - dynamic lib has no configs and is called libffi.
    if name == "Cffi" and mode == "dynamic":
        libname = "ffi"
        configs = [""]
    else:
        libname = name
    libnames = [libname + config for config in configs]
    # Special case libCffi - dynamic lib has no version suffix.
    if mode == "dynamic" and name != "Cffi":
        libnames = [libname + "-ghc*" for libname in libnames]
    if mode == "dynamic":
        exts = ["so", "so.*", "dylib", "dll"]
    else:
        exts = ["a"]
    return [
        "lib{}.{}".format(libname, ext)
        for libname in libnames
        for ext in exts
    ]

output = []

# Accumulate package id to package name mappings.
pkg_id_map = []
for conf in glob.glob(os.path.join(topdir, "package.conf.d", "*.conf")):
    with open(conf, 'r') as f:
        pkg = package_configuration.parse_package_configuration(f)

    # pkgroot is not part of .conf files. It's a computed value. It is
    # defined to be the directory enclosing the package database
    # directory.
    pkgroot = os.path.dirname(os.path.dirname(os.path.realpath(conf)))

    pkg_id_map.append((pkg.name, pkg.id))

    # Haddock handling
    # Haddock files may be packaged inside your ghc distribution (as
    # it is the case for ghc_bindist) or elsewhere if you pull GHC
    # from something such as nixpkgs which split ghc from its
    # documentation

    # For each path in haddock_html and haddock_interfaces, we will
    # first try to convert it to a path in the current WORKSPACE. if
    # this does not work, we generate a path in the workspace and
    # output a SYMLINK information for the parent process

    # first, try to get a path within the package
    # We check if the file exists because cabal will unconditionally
    # generate the database entry even if no haddock was generated.
    haddock_html = None
    if pkg.haddock_html:
        haddock_html = path_to_label(pkg.haddock_html, pkgroot)
        if not haddock_html:
            haddock_html = os.path.join("haddock", "html", pkg.name)
            output.append("#SYMLINK: {} {}".format(pkg.haddock_html, haddock_html))

    # If there is many interfaces, we give them a number
    interface_id = 0
    haddock_interfaces = []
    for interface_path in pkg.haddock_interfaces:
        interface = path_to_label(interface_path, pkgroot)
        if not interface:
            interface = os.path.join(
                "haddock",
                "interfaces",
                pkg.name + "_" + str(interface_id) + ".haddock",
            )
            output.append("#SYMLINK: {} {}".format(interface_path, interface))
            interface_id += 1
        haddock_interfaces.append(interface)

    output += [
        # We substitute globs instead of actual paths because the
        # libraries could be anywhere in the various library dirs, in
        # static or shared form, or neither.
        textwrap.dedent("""\
            haskell_import(
                name = "{name}",
                id = "{id}",
                deps = {deps},
                hdrs = {hdrs},
                includes = {includes},
                linkopts = {linkopts},
                shared_libraries = {shared_libraries},
                static_libraries = {static_libraries},
                static_profiling_libraries = {static_profiling_libraries},
                version = "{version}",
                visibility = ["//visibility:public"],
                haddock_interfaces = {haddock_interfaces},
                haddock_html = {haddock_html},
            )
            """.format(
                name = pkg.name,
                id = pkg.id,
                version = pkg.version,
                hdrs = "glob({}, allow_empty = True)".format([
                    path_to_label("{}/**/*.h".format(include_dir), pkgroot)
                    for include_dir in pkg.include_dirs
                    if path_to_label(include_dir, pkgroot)
                ]),
                includes = [
                    "/".join([repo_dir, path_to_label(include_dir, pkgroot)])
                    for include_dir in pkg.include_dirs
                    if path_to_label(include_dir, pkgroot)
                ],
                static_libraries = "glob({}, allow_empty = True)".format([
                    path_to_label("{}/{}".format(library_dir, pattern), pkgroot)
                    for hs_library in pkg.hs_libraries
                    for pattern in hs_library_pattern(hs_library, mode = "static", profiling = False)
                    for library_dir in pkg.library_dirs
                    if path_to_label(library_dir, pkgroot)
                ]),
                static_profiling_libraries = "glob({}, allow_empty = True)".format([
                    path_to_label("{}/{}".format(library_dir, pattern), pkgroot)
                    for hs_library in pkg.hs_libraries
                    for pattern in hs_library_pattern(hs_library, mode = "static", profiling = True)
                    for library_dir in pkg.library_dirs
                    if path_to_label(library_dir, pkgroot)
                ]),
                shared_libraries = "glob({}, allow_empty = True)".format([
                    path_to_label("{}/{}".format(dynamic_library_dir, pattern), pkgroot)
                    for hs_library in pkg.hs_libraries
                    for pattern in hs_library_pattern(hs_library, mode = "dynamic", profiling = False)
                    for dynamic_library_dir in pkg.dynamic_library_dirs + pkg.library_dirs
                    if path_to_label(dynamic_library_dir, pkgroot)
                ]),
                haddock_html = repr(haddock_html),
                haddock_interfaces = repr(haddock_interfaces),
                deps = pkg.depends,
                linkopts = pkg.ld_options + [
                    "-L{}".format(library_dir)
                    for library_dir in pkg.dynamic_library_dirs + pkg.library_dirs
                    if not path_to_label(library_dir, pkgroot)
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
