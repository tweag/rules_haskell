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
import json
from pathlib import Path

import package_configuration

def match_glob(root_dir, pattern):
    return sorted([p.relative_to(root_dir).as_posix() for p in Path(root_dir).glob(pattern)])

if len(sys.argv) == 3:
    repo_dir = "external/" + sys.argv[1]
    topdir = sys.argv[2]

    if os.path.exists(os.path.join(topdir, 'package.conf.d')):
        package_conf_dir = os.path.join(topdir, 'package.conf.d')
    elif os.path.exists(os.path.join(topdir, 'lib', 'package.conf.d')):
        topdir = os.path.join(topdir, 'lib')
        package_conf_dir = os.path.join(topdir, 'package.conf.d')
    else:
        sys.exit("could not find package.conf.d directory at {}".format(topdir))
    repo_root = os.getcwd()
else:
    sys.exit("Usage: pkgdb_to_bzl.py <REPO_NAME> <TOPDIR>")

def resolve(path, pkgroot):
    """Resolve references to ${pkgroot} with the given value, resolve $topdir with `topdir`"""
    if path.find("${pkgroot}") != -1:
        norm_path = os.path.normpath(path.strip("\"").replace("${pkgroot}", pkgroot))
        if not os.path.isabs(norm_path) and norm_path.startswith('..'):
            return resolve(path, os.path.realpath(pkgroot))
        else:
            return norm_path
    elif path.startswith("$topdir"):
        return os.path.normpath(path.replace("$topdir", topdir)).replace('\\', '/')
    else:
        return path


def join_paths(paths):
    return ["/".join(ps) for ps in paths if not None in ps]


symlinks = {}

def path_to_label(path, pkgroot, output=None):
    """Substitute one pkgroot for another relative one to obtain a label."""
    if path.find("${pkgroot}") != -1:
        # determine if the given path is inside the repository root
        # if it is not, return None to signal it needs to be symlinked into the
        # repository
        norm_path = os.path.normpath(resolve(path, pkgroot))
        relative_path = os.path.relpath(norm_path, start=repo_root)

        return None if relative_path.startswith('..') else relative_path.replace('\\', '/')

    topdir_relative_path = path.replace(os.path.realpath(pkgroot), "$topdir")
    if topdir_relative_path.startswith("$topdir"):
        return os.path.normpath(topdir_relative_path.replace("$topdir", topdir)).replace('\\', '/')

    if not output is None:
        if os.path.isabs(path) and os.path.exists(path):
            global symlinks
            lnk = symlinks.get(path)
            if not lnk:
                lnk = "lnk_{}".format(len(symlinks))
                symlinks[path] = lnk
                output.append("#SYMLINK: {} {}".format(path.replace('\\', '/'), lnk))
            return lnk
        else:
            print("WARN: could not handle", path, file=sys.stderr)

def hs_library_pattern(package_name, name, mode = "static", profiling = False):
    """Convert hs-libraries entry to glob patterns.

    Args:
        package_name: The name of the package.
        name: The library name. E.g. HSrts or Cffi.
        mode: The linking mode. Either "static" or "dynamic".
        profiling: Look for profiling mode libraries.

    Returns:
        List of globbing patterns for the library file.

    """
    configs = ["_p"] if profiling else [""]

    # Library names must either be prefixed with "HS" or "C" and corresponding
    # library file names must match:
    # - Libraries with name "HS<library-name>":
    #    - `libHS<library-name>.a`
    #    - `libHS<library-name>-ghc<ghc-flavour><ghc-version>.<dyn-library-extension>*`
    # - Libraries with name "C<library-name>":
    #    - `libC<library-name>.a`
    #    - `lib<library-name>.<dyn-library-extension>*`
    if name.startswith("C"):
        libname = name[1:] if mode == "dynamic" else name
    elif name.startswith("HS"):
        libname = name
    else:
        sys.error("do not know how to handle hs-library `{}` in package {}".format(name, package_name))

    # The RTS configuration suffix.
    # See https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/config#rts-configurations
    # Special case for rts - include multi threaded and single threaded, and debug / non-debug variants
    if package_name == "rts":
        configs = [
            prefix + config
            for config in configs
            for prefix in ["", "_thr", "_debug", "_thr_debug"]
        ]

    libnames = [libname + config for config in configs]

    if mode == "dynamic":
        libnames = [libname + "-ghc*" for libname in libnames]
        exts = ["so", "so.*", "dylib", "dll"] if mode == "dynamic" else ["a"]
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

for conf in glob.glob(os.path.join(package_conf_dir, '*.conf')):
    with open(conf, 'r') as f:
        pkg = package_configuration.parse_package_configuration(f)

    # pkgroot is not part of .conf files. It's a computed value. It is
    # defined to be the directory enclosing the package database
    # directory.
    pkgroot = os.path.dirname(os.path.dirname(conf))

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
    haddock_html = None

    if pkg.haddock_html:
        # We check if the file exists because cabal will unconditionally
        # generate the database entry even if no haddock was generated.
        resolved_haddock_html = resolve(pkg.haddock_html, pkgroot)

        if not os.path.exists(resolved_haddock_html):
            # try to resolve relative to the package.conf.d dir
            # see https://gitlab.haskell.org/ghc/ghc/-/issues/23476
            resolved_haddock_html = resolve(pkg.haddock_html, package_conf_dir)

        if os.path.exists(resolved_haddock_html):
            haddock_html = path_to_label(pkg.haddock_html, pkgroot)
            if not haddock_html:
                haddock_html = os.path.join("haddock", "html", pkg.name)
                output.append("#SYMLINK: {} {}".format(resolved_haddock_html.replace('\\', '/'), haddock_html))

    # If there is many interfaces, we give them a number
    interface_id = 0
    haddock_interfaces = []
    for interface_path in pkg.haddock_interfaces:
        resolved_path = resolve(interface_path, pkgroot).replace('\\', '/')

        if not os.path.exists(resolved_path):
            # try to resolve relative to the package.conf.d dir
            # see https://gitlab.haskell.org/ghc/ghc/-/issues/23476
            resolved_path = resolve(interface_path, package_conf_dir)

        # We check if the file exists because cabal will unconditionally
        # generate the database entry even if no haddock was generated.
        if not os.path.exists(resolved_path): continue

        interface = path_to_label(interface_path, pkgroot)

        if not interface:
            interface = os.path.join(
                "haddock",
                "interfaces",
                pkg.name + "_" + str(interface_id) + ".haddock",
            )
            output.append("#SYMLINK: {} {}".format(resolved_path, interface))
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
                hdrs = join_paths([
                    [path_to_label(include_dir, pkgroot, output), header]
                    for include_dir in pkg.include_dirs
                    for header in match_glob(resolve(include_dir, pkgroot), "**/*.h")
                ]),
                includes = join_paths([
                    [repo_dir, path_to_label(include_dir, pkgroot, output)]
                    for include_dir in pkg.include_dirs
                ]),
                static_libraries = join_paths([
                    [path_to_label(library_dir, pkgroot, output), library]
                    for hs_library in pkg.hs_libraries
                    for pattern in hs_library_pattern(pkg.name, hs_library, mode = "static", profiling = False)
                    for library_dir in pkg.library_dirs
                    for library in match_glob(resolve(library_dir, pkgroot), pattern)
                ]),
                static_profiling_libraries = join_paths([
                    [path_to_label(library_dir, pkgroot, output), library]
                    for hs_library in pkg.hs_libraries
                    for pattern in hs_library_pattern(pkg.name, hs_library, mode = "static", profiling = True)
                    for library_dir in pkg.library_dirs
                    for library in match_glob(resolve(library_dir, pkgroot), pattern)
                ]),
                shared_libraries = join_paths([
                    [path_to_label(dynamic_library_dir, pkgroot, output), library]
                    for hs_library in pkg.hs_libraries
                    for pattern in hs_library_pattern(pkg.name, hs_library, mode = "dynamic", profiling = False)
                    for dynamic_library_dir in set(pkg.dynamic_library_dirs + pkg.library_dirs)
                    for library in match_glob(resolve(dynamic_library_dir, pkgroot), pattern)
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

# The _ahc_impl function recovers this toolchain_libraries variable and assumes it is defined on a single line.
toolchain_libraries = textwrap.dedent("{pkgs}".format(pkgs = [pkg_name for pkg_name, _ in pkg_id_map])
    )
output.append("toolchain_libraries = {}".format(toolchain_libraries))

result = {
    "file_content": "\n".join(output),
    "toolchain_libraries": toolchain_libraries,
}
print(json.dumps(result))
