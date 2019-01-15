#!/usr/bin/env bash
#
# Usage: haddock-wrapper.sh <PREBUILD_DEPS_FILE> <HADDOCK_ARGS>

set -eo pipefail

%{env}

PREBUILT_DEPS_FILE=$1
shift

extra_args=()

for pkg in $(< $PREBUILT_DEPS_FILE)
do
    # Assumption: the `haddock-interfaces` field always only contains
    # exactly one file name. This seems to hold in practice, though the
    # ghc documentation defines it as:
    # > (string list) A list of filenames containing Haddock interface files
    # > (.haddock files) for this package.
    # If there were more than one file, going by the output for the `depends`,
    # the file names would be separated by a space character.
    # https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/packages.html#installedpackageinfo-a-package-specification
    haddock_interfaces=$(%{ghc-pkg} --simple-output field $pkg haddock-interfaces)
    haddock_html=$(%{ghc-pkg} --simple-output field $pkg haddock-html)

    # Sometimes the referenced `.haddock` file does not exist
    # (e.g. for `nixpkgs.haskellPackages` deps with haddock disabled).
    # In that case, skip this package with a warning.
    if [[ -f "$haddock_interfaces" ]]
    then
        # TODO: link source code,
        # `--read-interface=$haddock_html,$pkg_src,$haddock_interfaces
        # https://haskell-haddock.readthedocs.io/en/latest/invoking.html#cmdoption-read-interface
        extra_args+=("--read-interface=$haddock_html,$haddock_interfaces")
    else
        echo "Warning: haddock missing for package $pkg" 1>&2
    fi
done

# BSD and GNU mktemp are very different; attempt GNU first
TEMP=$(mktemp -d 2>/dev/null || mktemp -d -t 'haddock_wrapper')
trap cleanup 1 2 3 6
cleanup() { rmdir "$TEMP"; }
# XXX Override TMPDIR to prevent race conditions on certain platforms.
# This is a workaround for
# https://github.com/haskell/haddock/issues/894.
TMPDIR=$TEMP %{haddock} "${extra_args[@]}" "$@"
cleanup
