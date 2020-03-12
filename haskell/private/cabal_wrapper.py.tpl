#!/usr/bin/env python3

# cabal_wrapper.py <COMPONENT> <PKG_NAME> <HADDOCK> <SETUP_PATH> <PKG_DIR> <PACKAGE_DB_PATH> [EXTRA_ARGS...] -- [PATH_ARGS...]
#
# This wrapper calls Cabal's configure/build/install steps one big
# action so that we don't have to track all inputs explicitly between
# steps.
#
# COMPONENT: Cabal component to build.
# PKG_NAME: Package ID of the resulting package.
# HADDOCK: Whether to generate haddock documentation.
# SETUP_PATH: Path to Setup.hs
# PKG_DIR: Directory containing the Cabal file
# PACKAGE_DB_PATH: Output package DB path.
# EXTRA_ARGS: Additional args to Setup.hs configure.
# PATH_ARGS: Additional args to Setup.hs configure where paths need to be prefixed with execroot.

from __future__ import print_function

from bazel_tools.tools.python.runfiles import runfiles as bazel_runfiles
from contextlib import contextmanager
from glob import glob
import os
import os.path
import re
import shutil
import subprocess
import sys
import tempfile

debug = False
verbose = os.environ.get("CABAL_VERBOSE", "") == "True"

def run(cmd, *args, **kwargs):
    if debug:
        print("+ " + " ".join(["'{}'".format(arg) for arg in cmd]), file=sys.stderr)
        sys.stderr.flush()
    if verbose:
        subprocess.run(cmd, check=True, *args, **kwargs)
    else:
        try:
            subprocess.run(cmd, check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE, *args, **kwargs)
        except subprocess.CalledProcessError as err:
            sys.stdout.buffer.write(err.stdout)
            sys.stderr.buffer.write(err.stderr)
            raise

def find_exe(exe):
    if os.path.isfile(exe):
        path = os.path.abspath(exe)
    elif "%{is_windows}" == "True" and os.path.isfile(exe + ".exe"):
        path = os.path.abspath(exe + ".exe")
    else:
        r = bazel_runfiles.Create()
        path = r.Rlocation("%{workspace}/" + exe)
        if not os.path.isfile(path) and "%{is_windows}" == "True":
            path = r.Rlocation("%{workspace}/" + exe + ".exe")
    return path

path_list_sep = ";" if "%{is_windows}" == "True" else ":"

def canonicalize_path(path):
    return path_list_sep.join([
        os.path.abspath(entry)
        for entry in path.split(path_list_sep)
        if entry != ""
    ])

# Remove any relative entries, because we'll be changing CWD shortly.
os.environ["LD_LIBRARY_PATH"] = canonicalize_path(os.getenv("LD_LIBRARY_PATH", ""))
os.environ["LIBRARY_PATH"] = canonicalize_path(os.getenv("LIBRARY_PATH", ""))
os.environ["PATH"] = canonicalize_path(os.getenv("PATH", ""))

component = sys.argv.pop(1)
name = sys.argv.pop(1)
haddock = sys.argv.pop(1) == "true"
execroot = os.getcwd()
setup = os.path.join(execroot, sys.argv.pop(1))
srcdir = os.path.join(execroot, sys.argv.pop(1))
# By definition (see ghc-pkg source code).
pkgroot = os.path.realpath(os.path.join(execroot, os.path.dirname(sys.argv.pop(1))))
libdir = os.path.join(pkgroot, "{}_iface".format(name))
dynlibdir = os.path.join(pkgroot, "lib")
bindir = os.path.join(pkgroot, "bin")
datadir = os.path.join(pkgroot, "{}_data".format(name))
package_database = os.path.join(pkgroot, "{}.conf.d".format(name))
haddockdir = os.path.join(pkgroot, "{}_haddock".format(name))
htmldir = os.path.join(pkgroot, "{}_haddock_html".format(name))

runghc = find_exe(r"%{runghc}")
ghc = find_exe(r"%{ghc}")
ghc_pkg = find_exe(r"%{ghc_pkg}")

extra_args = []
current_arg = sys.argv.pop(1)
while current_arg != "--":
    extra_args.append(current_arg)
    current_arg = sys.argv.pop(1)
del current_arg

path_args = sys.argv[1:]

ar = find_exe("%{ar}")
cc = find_exe("%{cc}")
strip = find_exe("%{strip}")

def recache_db():
    run([ghc_pkg, "recache", "--package-db=" + package_database])

recache_db()

@contextmanager
def tmpdir():
    """This is a reimplementation of `tempfile.TemporaryDirectory` because
    the latter isn't available in python2
    """
    distdir = tempfile.mkdtemp()
    try:
        yield distdir
    finally:
        shutil.rmtree(distdir, ignore_errors = True)

with tmpdir() as distdir:
    enable_relocatable_flags = ["--enable-relocatable"] \
            if "%{is_windows}" != "True" else []

    # Cabal really wants the current working directory to be directory
    # where the .cabal file is located. So we have no choice but to chance
    # cd into it, but then we have to rewrite all relative references into
    # absolute ones before doing so (using $execroot).
    old_cwd = os.getcwd()
    os.chdir(srcdir)
    os.putenv("HOME", "/var/empty")
    os.putenv("TMPDIR", os.path.join(distdir, "tmp"))
    os.putenv("TMP", os.path.join(distdir, "tmp"))
    os.putenv("TEMP", os.path.join(distdir, "tmp"))
    os.makedirs(os.path.join(distdir, "tmp"))
    run([runghc, setup, "configure", \
        component, \
        "--verbose=0", \
        "--user", \
        "--with-compiler=" + ghc,
        "--with-hc-pkg=" + ghc_pkg,
        "--with-ar=" + ar,
        "--with-gcc=" + cc,
        "--with-strip=" + strip,
        "--enable-deterministic", \
        ] +
        enable_relocatable_flags + \
        [ \
        "--builddir=" + distdir, \
        "--prefix=" + pkgroot, \
        "--libdir=" + libdir, \
        "--dynlibdir=" + dynlibdir, \
        "--libsubdir=", \
        "--bindir=" + bindir, \
        "--datadir=" + datadir, \
        # Note, setting --datasubdir is required to work around
        #   https://github.com/haskell/cabal/issues/6235
        "--datasubdir=", \
        "--haddockdir=" + haddockdir, \
        "--htmldir=" + htmldir, \
        "--package-db=clear", \
        "--package-db=global", \
        ] + \
        extra_args + \
        [ arg.replace("=", "=" + execroot + "/") for arg in path_args ] + \
        [ "--package-db=" + package_database ], # This arg must come last.
        )
    run([runghc, setup, "build", "--verbose=0", "--builddir=" + distdir])
    if haddock:
        run([runghc, setup, "haddock", "--verbose=0", "--builddir=" + distdir])
    run([runghc, setup, "install", "--verbose=0", "--builddir=" + distdir])
    # Bazel builds are not sandboxed on Windows and can be non-sandboxed on
    # other OSs. Operations like executing `configure` scripts can modify the
    # source tree. If the `srcs` attribute uses a glob like `glob(["**"])`,
    # then these modified files will enter `srcs` on the next execution and
    # invalidate the cache. To avoid this we remove generated files.
    run([runghc, setup, "clean", "--verbose=0", "--builddir=" + distdir])
    os.chdir(old_cwd)

# XXX Cabal has a bizarre layout that we can't control directly. It
# confounds the library-dir and the import-dir (but not the
# dynamic-library-dir). That's pretty annoying, because Bazel won't
# allow overlap in the path to the interface files directory and the
# path to the static library. So we move the static library elsewhere
# and patch the .conf file accordingly.
#
# There were plans for controlling this, but they died. See:
# https://github.com/haskell/cabal/pull/3982#issuecomment-254038734
libraries=glob(os.path.join(libdir, "libHS*.a"))
package_conf_file = os.path.join(package_database, name + ".conf")

def make_relocatable_paths(line):
    line = re.sub("library-dirs:.*", "library-dirs: ${pkgroot}/lib", line)

    def make_relative_to_pkgroot(matchobj):
        abspath=matchobj.group(0)
        return os.path.join("${pkgroot}", os.path.relpath(abspath, start=pkgroot))

    # The $execroot is an absolute path and should not leak into the output.
    # Replace each ocurrence of execroot by a path relative to ${pkgroot}.
    line = re.sub(re.escape(execroot) + '\S*', make_relative_to_pkgroot, line)
    return line

if libraries != [] and os.path.isfile(package_conf_file):
    for lib in libraries:
        os.rename(lib, os.path.join(dynlibdir, os.path.basename(lib)))

    tmp_package_conf_file = package_conf_file + ".tmp"
    with open(package_conf_file, 'r', errors='surrogateescape') as package_conf:
        with open(tmp_package_conf_file, 'w', errors='surrogateescape') as tmp_package_conf:
            for line in package_conf.readlines():
                print(make_relocatable_paths(line), file=tmp_package_conf)
    os.remove(package_conf_file)
    os.rename(tmp_package_conf_file, package_conf_file)
    recache_db()
