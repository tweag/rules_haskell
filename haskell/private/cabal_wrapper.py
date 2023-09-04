#!/usr/bin/env python3

# cabal_wrapper.py <FILE.JSON>
#
# This wrapper calls Cabal's configure/build/install steps one big
# action so that we don't have to track all inputs explicitly between
# steps. It receives the path to a json file with the following schema:
#
# { "component": string           # Cabal component to build.
# , "pkg_name": string            # Package ID of the resulting package.
# , "generate_haddock": boolean   # Whether to generate haddock documentation.
# , "setup_path": string          # Path to Setup.hs
# , "pkg_dir": string             # Directory containing the Cabal file
# , "package_db_path": string     # Output package DB path.
# , "runghc_args": list of string # Arguments for runghc
# , "extra_args": list of string  # Additional args to Setup.hs configure.
# , "path_args": list of string   # Additional args to Setup.hs configure where paths need to be prefixed with execroot.
# , "toolchain_info" :
#     { "ghc": string                  # path to ghc
#     , "hsc2hs": string               # path to hsc2hs
#     , "ghc_pkg": string              # path to ghc_pkg
#     , "runghc": string               # path to runghc
#     , "ar": string                   # path to ar
#     , "cc": string                   # path to cc
#     , "strip": string                # path to strip
#     , "is_windows": boolean          # this is a windows build
#     , "workspace": string            # workspace name
#     , "ghc_cc_args":  list of string # cc flags for ghc
#     }
# , "generate_paths_module": boolean # whether to generate a paths_module
# , "ghc_version": List of int       # version of ghc
# , "cabal_basename": basename of cabal binary
# , "cabal_dirname": dirname of cabal binary
# , "extra_ldflags_file": File with extra flags for ld or null
# }

from __future__ import print_function

from contextlib import contextmanager
from glob import glob
import itertools
import json
import hashlib
import os
import os.path
import re
import shutil
import subprocess
import sys
import tempfile
from generate_cabal_paths_module import generate_cabal_paths_module

debug = False
verbose = os.environ.get("CABAL_VERBOSE", "") == "True"
with open(sys.argv.pop(1)) as json_file:
    json_args = json.load(json_file)

toolchain_info = json_args["toolchain_info"]
is_windows = toolchain_info["is_windows"]

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
    elif is_windows and os.path.isfile(exe + ".exe"):
        path = os.path.abspath(exe + ".exe")
    else:
        path = toolchain_info["workspace"] + "/" + exe
        if not os.path.isfile(path) and is_windows:
            path = toolchain_info["workspace"] + "/" + exe + ".exe"
    return path

path_list_sep = ";" if is_windows else ":"

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
os.environ["RULES_HASKELL_GHC_PATH"] = canonicalize_path(os.getenv("RULES_HASKELL_GHC_PATH", ""))
os.environ["RULES_HASKELL_GHC_PKG_PATH"] = canonicalize_path(os.getenv("RULES_HASKELL_GHC_PKG_PATH", ""))
os.environ["RULES_HASKELL_LIBDIR_PATH"] = canonicalize_path(os.getenv("RULES_HASKELL_LIBDIR_PATH", ""))
os.environ["RULES_HASKELL_DOCDIR_PATH"] = canonicalize_path(os.getenv("RULES_HASKELL_DOCDIR_PATH", ""))

component = json_args["component"]
name = json_args["pkg_name"]
haddock = json_args["generate_haddock"]
extra_ldflags_file = json_args["extra_ldflags_file"]
execroot = os.getcwd()
setup = os.path.join(execroot, json_args["setup_path"])
srcdir = os.path.join(execroot, json_args["pkg_dir"])
# By definition (see ghc-pkg source code).
rel_pkgroot = os.path.dirname(json_args["package_db_path"])
pkgroot = os.path.realpath(os.path.join(execroot, rel_pkgroot))
libdir = os.path.join(pkgroot, "{}_iface".format(name))
dynlibdir = os.path.join(pkgroot, "lib")
bindir = os.path.join(pkgroot, "bin")
datadir = os.path.join(pkgroot, "{}_data".format(name))
package_database = os.path.join(pkgroot, "{}.conf.d".format(name))
haddockdir = os.path.join(pkgroot, "{}_haddock".format(name))
htmldir = os.path.join(pkgroot, "{}_haddock_html".format(name))
runghc_args = json_args["runghc_args"]
deps_package_databases = json_args["package_databases"]

runghc = find_exe(toolchain_info["runghc"])
ghc = find_exe(toolchain_info["ghc"])
hsc2hs = find_exe(toolchain_info["hsc2hs"])
ghc_pkg = find_exe(toolchain_info["ghc_pkg"])

extra_args = json_args["extra_args"]

path_args = json_args["path_args"]

ar = find_exe(toolchain_info["ar"])
cc = find_exe(toolchain_info["cc"])
ld = find_exe(toolchain_info["ld"])
strip = find_exe(toolchain_info["strip"])

def recache_db():
    run([ghc_pkg, "recache", "--package-db=" + package_database])

recache_db()

@contextmanager
def mkdtemp(prefix):
    """Create a temporary directory.

    This is a context manager that will create the directory on entry and
    delete it on exit.

    The directory will be created under the given `prefix` path with an
    optional suffix to avoid conflict with already existing directories.
    """
    candidates = itertools.chain([prefix], ("{}_{}".format(prefix, i) for i in itertools.count(1)))
    for candidate in candidates:
        dirname = os.path.abspath(candidate)
        try:
            os.makedirs(dirname, mode=0o700, exist_ok=False)
            break
        except FileExistsError:
            pass
    try:
        yield dirname
    finally:
        shutil.rmtree(dirname, ignore_errors = True)

@contextmanager
def init_deps_db():
    """ Optionally initialise a package_db in which to add dependencies.
    In order to keep command lines shorter on windows.

    This is a context manager that will create the directory on entry and
    delete it on exit.
    """
    if is_windows and deps_package_databases:
        deps_package_root = os.path.dirname(pkgroot)
        deps_package_db = os.path.join(deps_package_root, "rules_haskell_deps_of_{}.conf.d".format(name))
        candidates = (os.path.join(deps_package_root, "rules_haskell_deps_of_{}_{}".format(name, i)) for i in itertools.count(1))
        for candidate in candidates:
            try:
                os.makedirs(candidate, mode=0o700, exist_ok=False)
                run([ghc_pkg, "recache","--package-db={}".format(candidate)])
                break
            except FileExistsError:
                pass
        try:
            yield candidate
        finally:
            shutil.rmtree(candidate, ignore_errors = True)
    else:
        yield None

def distdir_prefix():
    # Build into a sibling path of the final binary output location.
    # This is to ensure that relative `RUNPATH`s are valid in the intermediate
    # output in the `--builddir` as well as in the final output in `--bindir`.
    # Executables are placed into `<distdir>/build/<package-name>/<binary>`.
    # Libraries are placed into `<distdir>/build/<library>`. I.e. there is an
    # extra subdirectory for libraries.
    if is_windows:
        # On Windows we don't do dynamic linking and prefer shorter paths to
        # avoid exceeding `MAX_PATH`.
        distdir_root = tempfile.gettempdir()
    else:
        if component.startswith("exe:"):
            distdir_root = os.path.dirname(os.path.dirname(pkgroot))
        else:
            distdir_root = os.path.dirname(pkgroot)
    if is_windows:
        # On Windows we use a fixed length directory name to avoid exceeding
        # `MAX_PATH` on targets with long names.
        distdir_name = hashlib.md5(name.encode("utf-8")).hexdigest()
    else:
        distdir_name = name
    return os.path.join(distdir_root, name)

# Build into a temporary distdir that will be cleaned up after the build. The
# path to this distdir enters into flags that are passed to GHC and thereby
# into the 'flag hash' field of generated interface files. We try to use a
# reproducible path for the distdir to keep interface files reproducible.
with mkdtemp(distdir_prefix()) as distdir, init_deps_db() as deps_package_db:
    enable_relocatable_flags = ["--enable-relocatable"] \
            if not is_windows else []

    # Cabal really wants the current working directory to be directory
    # where the .cabal file is located. So we have no choice but to chance
    # cd into it, but then we have to rewrite all relative references into
    # absolute ones before doing so (using $execroot).
    old_cwd = os.getcwd()
    os.chdir(srcdir)
    os.putenv("RULES_HASKELL_EXEC_ROOT", old_cwd)
    os.putenv("HOME", "/var/empty")
    os.putenv("TMPDIR", os.path.join(distdir, "tmp"))
    os.putenv("TMP", os.path.join(distdir, "tmp"))
    os.putenv("TEMP", os.path.join(distdir, "tmp"))
    os.makedirs(os.path.join(distdir, "tmp"))

    rel_execroot = os.path.relpath(execroot)
    cfg_execroot = rel_execroot if not is_windows else execroot
    cfg_pkgroot = os.path.join(cfg_execroot, rel_pkgroot)

    if deps_package_db:
        # We merge databases of dependencies together assuming that they each contain only one package
        for db in deps_package_databases:
            try:
                ps = subprocess.Popen([ghc_pkg, "dump", "--expand-pkgroot", "-f", os.path.dirname(os.path.join(execroot,db))], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
                subprocess.check_output([ghc_pkg, "update", "-f", deps_package_db, "-"], stdin=ps.stdout, stderr=subprocess.PIPE)
                ps.wait()
            except subprocess.CalledProcessError as err:
                sys.stdout.buffer.write(err.stdout)
                sys.stderr.buffer.write(err.stderr)
                raise

    # Create a Paths module that will be used instead of the cabal generated one.
    # https://cabal.readthedocs.io/en/3.4/cabal-package.html#accessing-data-files-from-package-code
    generated_paths_file = None
    if json_args["generate_paths_module"]:
        component_name = component.split(':')[1]
        (paths_file, cabal_paths_file_content) = generate_cabal_paths_module(
            component_name = component_name,
            ghc_version = json_args["ghc_version"],
            is_windows = is_windows,
            cabal_basename = json_args["cabal_basename"],
            cabal_dirname = json_args["cabal_dirname"],
            ghc = ghc,
            libdir = os.path.basename(libdir),
            dynlibdir = os.path.basename(dynlibdir),
            bindir = os.path.basename(bindir),
            datadir = os.path.basename(datadir),
            pkgroot = pkgroot,
            workspace = toolchain_info["workspace"],
        )
        if not os.path.exists(paths_file):
            with open(paths_file, 'w') as f:
                f.write(cabal_paths_file_content)
            generated_paths_file = paths_file

    # XXX: Bazel hack
    # When cabal_wrapper calls other tools with runfiles, the runfiles are
    # searched in the runfile tree of cabal_wrapper unless we clear
    # RUNFILES env vars. After clearing the env vars, each tool looks for
    # runfiles in its own runfiles tree.
    #
    # Clearing RUNFILES_DIR is necessary in macos where a wrapper script
    # cc-wrapper.sh is used from the cc toolchain.
    #
    # Clearing RUNFILES_MANIFEST_FILE is necessary in windows where we
    # use a wrapper script cc-wrapper-bash.exe which has a different
    # manifest file than cabal_wrapper.py.
    if "RUNFILES_DIR" in os.environ:
        del os.environ["RUNFILES_DIR"]
    if "RUNFILES_MANIFEST_FILE" in os.environ:
        del os.environ["RUNFILES_MANIFEST_FILE"]
    runghc_args = [arg.replace("./", execroot + "/") for arg in runghc_args]

    if not deps_package_db:
        path_args.extend(["--package-db=" + os.path.dirname(db) for db in deps_package_databases])
    path_args = [ arg.replace("=", "=" + cfg_execroot + "/") for arg in path_args ]
    if deps_package_db:
        path_args.append("--package-db={}".format(deps_package_db))

    run([runghc] + runghc_args + [setup, "configure", \
        component, \
        "--verbose=0", \
        "--user", \
        "--with-compiler=" + ghc,
        "--with-hc-pkg=" + ghc_pkg,
        "--with-hsc2hs=" + hsc2hs,
        "--with-ar=" + ar,
        "--with-gcc=" + cc,
        "--with-ld=" + ld,
        "--with-strip=" + strip,
        "--enable-deterministic", \
        ] +
        [ "--ghc-option=" + flag.replace("$CC", cc).replace("$LD", ld) for flag in toolchain_info["ghc_cc_args"] ] +
        [ "--hsc2hs-option=-c" + cc,
          "--hsc2hs-option=-l" + cc,
        ] +
        [ "--ghc-option=-optl@" + os.path.join(cfg_execroot, f) for f in [extra_ldflags_file] if f ] +
        enable_relocatable_flags + \
        [ \
        # Make `--builddir` a relative path. Using an absolute path would
        # confuse the `RUNPATH` patching logic in `cc_wrapper`. It assumes that
        # absolute paths refer the temporary directory that GHC uses for
        # intermediate template Haskell outputs. `cc_wrapper` should improved
        # in that regard.
        "--builddir=" + (os.path.relpath(distdir) if not is_windows else distdir), \
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
        path_args +\
        [ "--package-db=" + package_database ], # This arg must come last.
        )
    run([runghc] + runghc_args + [setup, "build", "--verbose=0", "--builddir=" + distdir])
    if haddock:
        run([runghc] + runghc_args + [setup, "haddock", "--verbose=0", "--builddir=" + distdir])
    # Setup.hs install fails if the configuration uses relative C header or
    # library search paths. Instead, we use Setup.hs copy to install the
    # package files and then manually register the package configuration in the
    # package-db.
    # See https://github.com/haskell/cabal/issues/1317#issuecomment-1025942396
    run([runghc] + runghc_args + [setup, "copy", "--verbose=0", "--builddir=" + distdir])
    if component.startswith("lib"):
        run([runghc] + runghc_args + [setup, "register", "--gen-pkg-config=" + os.path.join(package_database, name + ".conf"), "--verbose=0", "--builddir=" + distdir])
    # Bazel builds are not sandboxed on Windows and can be non-sandboxed on
    # other OSs. Operations like executing `configure` scripts can modify the
    # source tree. If the `srcs` attribute uses a glob like `glob(["**"])`,
    # then these modified files will enter `srcs` on the next execution and
    # invalidate the cache. To avoid this we remove generated files.
    run([runghc] + runghc_args + [setup, "clean", "--verbose=0", "--builddir=" + distdir])
    if generated_paths_file: os.remove(generated_paths_file)
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
        oldpath=matchobj.group(0)
        rel_to_pkgroot = os.path.relpath(oldpath, start=cfg_pkgroot)
        return os.path.join("${pkgroot}", rel_to_pkgroot)

    # On Windows paths may contain the absolute path to the execroot.
    # This path is non-reproducible and should not leak into build outputs.
    #
    # On other systems paths may be relative to the distdir.
    # These paths are invalid in the package configuration.
    #
    # Replace each ocurrence of either kind of path by one relative to ${pkgroot}.
    line = re.sub(re.escape(cfg_execroot) + r'\S*', make_relative_to_pkgroot, line)
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
