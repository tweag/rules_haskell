#!/usr/bin/env bash
#
# cabal_wrapper.sh <PKG_NAME> <SETUP_PATH> <PKG_DIR> <PACKAGE_DB_PATH> [SETUP_ARGS...]
#
# This wrapper calls Cabal's configure/build/install steps one big
# action so that we don't have to track all inputs explicitly between
# steps.

# TODO Remove once https://github.com/bazelbuild/bazel/issues/5980 is
# fixed.
%{env}

set -euo pipefail
execroot="$(pwd)"

# Set these variables if unset.
: ${LD_LIBRARY_PATH:=}
: ${LIBRARY_PATH:=}
export LD_LIBRARY_PATH=$execroot/${LD_LIBRARY_PATH//:/:$execroot/}
export LIBRARY_PATH=$execroot/${LIBRARY_PATH//:/:$execroot/}

name=$1
setup=$execroot/$2
srcdir=$execroot/$3
pkgroot="$(realpath $execroot/$4/..)" # By definition (see ghc-pkg source code).
shift 4

distdir=$(mktemp -d)
libdir=$pkgroot/iface
dynlibdir=$pkgroot/lib
package_database=$pkgroot/package.conf.d

%{ghc_pkg} recache --package-db=$package_database

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
    --with-ar=%{ar} \
    --with-strip=%{strip} \
    --enable-deterministic \
    --enable-relocatable \
    --builddir=$distdir \
    --prefix=$pkgroot \
    --libdir=$libdir \
    --dynlibdir=$dynlibdir \
    --libsubdir= \
    --package-db=clear \
    --package-db=global \
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
mv $libdir/libHS*.a $dynlibdir
sed 's,library-dirs:.*,library-dirs: ${pkgroot}/lib,' \
    $package_database/$name.conf > $package_database/$name.conf.tmp
mv  $package_database/$name.conf.tmp $package_database/$name.conf
%{ghc_pkg} recache --package-db=$package_database
