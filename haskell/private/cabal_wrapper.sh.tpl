#!/usr/bin/env bash
#
# cabal_wrapper.sh <PKG_NAME> <SETUP_PATH> <PKG_DIR> <PACKAGE_DB_PATH> [EXTRA_ARGS...] -- [PATH_ARGS...]
#
# This wrapper calls Cabal's configure/build/install steps one big
# action so that we don't have to track all inputs explicitly between
# steps.
#
# PKG_NAME: Package ID of the resulting package.
# SETUP_PATH: Path to Setup.hs
# PKG_DIR: Directory containing the Cabal file
# PACKAGE_DB_PATH: Output package DB path.
# EXTRA_ARGS: Additional args to Setup.hs configure.
# PATH_ARGS: Additional args to Setup.hs configure where paths need to be prefixed with execroot.

# TODO Remove once https://github.com/bazelbuild/bazel/issues/5980 is
# fixed.
%{env}

set -euo pipefail
shopt -s nullglob

# Poor man's realpath, because realpath is not available on macOS.
function realpath()
{
    [[ $1 = /* ]] && echo "$1" || echo "$PWD/${1#./}"
}

function canonicalize_path()
{
    new_path=""
    while IFS=: read -r -d: entry
    do
        if [[ -n "$entry" ]]
        then
            new_path="$new_path${new_path:+:}$(realpath "$entry")"
        fi
    done <<< "${1:-}:"
    echo $new_path
}

# relative_to ORIGIN PATH
# Compute the relative path from ORIGIN to PATH.
function relative_to() {
    local out=
    # Split path into components
    local -a relto; IFS="/\\" read -ra relto <<<"$1"
    local -a path; IFS="/\\" read -ra path <<<"$2"
    local off=0
    while [[ "${relto[$off]}" == "${path[$off]}" ]]; do
        if [[ $off -eq ${#relto[@]} || $off -eq ${#path[@]} ]]; then
            break
        fi
        : $((off++))
    done
    for ((i=$off; i < ${#relto[@]}; i++)); do
        out="$out${out:+/}.."
    done
    for ((i=$off; i < ${#path[@]}; i++)); do
        out="$out${out:+/}${path[$i]}"
    done
    echo "$out"
}

# Remove any relative entries, because we'll be changing CWD shortly.
LD_LIBRARY_PATH=$(canonicalize_path $LD_LIBRARY_PATH)
LIBRARY_PATH=$(canonicalize_path $LIBRARY_PATH)
PATH=$(canonicalize_path $PATH)

name=$1
execroot="$(pwd)"
setup=$execroot/$2
srcdir=$execroot/$3
pkgroot="$(realpath $execroot/$(dirname $4))" # By definition (see ghc-pkg source code).
shift 4

declare -a extra_args
while [[ $1 != -- ]]; do
    extra_args+=("$1")
    shift 1
done
shift 1

ar=$(realpath %{ar})
strip=$(realpath %{strip})
distdir=$(mktemp -d)
libdir=$pkgroot/iface
dynlibdir=$pkgroot/lib
bindir=$pkgroot/bin
datadir=$pkgroot/data
package_database=$pkgroot/package.conf.d

%{ghc_pkg} recache --package-db=$package_database

ENABLE_RELOCATABLE=
if [[ %{is_windows} != True ]]; then
    ENABLE_RELOCATABLE=--enable-relocatable
fi

# Cabal really wants the current working directory to be directory
# where the .cabal file is located. So we have no choice but to chance
# cd into it, but then we have to rewrite all relative references into
# absolute ones before doing so (using $execroot).
cd $srcdir
export HOME=/var/empty
$execroot/%{runghc} $setup configure \
    --verbose=0 \
    --user \
    --with-compiler=$execroot/%{ghc} \
    --with-hc-pkg=$execroot/%{ghc_pkg} \
    --with-ar=$ar \
    --with-strip=$strip \
    --enable-deterministic \
    $ENABLE_RELOCATABLE \
    --builddir=$distdir \
    --prefix=$pkgroot \
    --libdir=$libdir \
    --dynlibdir=$dynlibdir \
    --libsubdir= \
    --bindir=$bindir \
    --datadir=$datadir \
    --package-db=clear \
    --package-db=global \
    "${extra_args[@]}" \
    "${@/=/=$execroot/}" \
    --package-db=$package_database # This arg must come last.
$execroot/%{runghc} $setup build --verbose=0 --builddir=$distdir
$execroot/%{runghc} $setup install --verbose=0 --builddir=$distdir
cd - >/dev/null

# XXX Cabal has a bizarre layout that we can't control directly. It
# confounds the library-dir and the import-dir (but not the
# dynamic-library-dir). That's pretty annoying, because Bazel won't
# allow overlap in the path to the interface files directory and the
# path to the static library. So we move the static library elsewhere
# and patch the .conf file accordingly.
#
# There were plans for controlling this, but they died. See:
# https://github.com/haskell/cabal/pull/3982#issuecomment-254038734
library=($libdir/libHS*.a)
if [[ -n ${library+x} && -f $package_database/$name.conf ]]
then
    mv $libdir/libHS*.a $dynlibdir
    # The $execroot is an absolute path and should not leak into the output.
    # Replace each ocurrence of execroot by a path relative to ${pkgroot}.
    function replace_execroot() {
        local line
        local relpath
        while IFS="" read -r line; do
            while [[ $line =~ ("$execroot"[^[:space:]]*) ]]; do
                relpath="$(relative_to "$pkgroot" "${BASH_REMATCH[1]}")"
                line="${line/${BASH_REMATCH[1]}/\$\{pkgroot\}\/$relpath/}"
            done
            echo "$line"
        done
    }
    sed -e 's,library-dirs:.*,library-dirs: ${pkgroot}/lib,' \
        $package_database/$name.conf \
        | replace_execroot \
        > $package_database/$name.conf.tmp
    mv  $package_database/$name.conf.tmp $package_database/$name.conf
    %{ghc_pkg} recache --package-db=$package_database
fi
