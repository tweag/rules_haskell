#!/usr/bin/env bash
#
# Usage: hoogle-wrapper.sh <HOOGLE_PATH> <HADDOCK_ARGS>

set -eo pipefail

hoogle=$1
shift
PREBUILT_DEPS_FILE=$1
shift

extra_args=()

for pkg in $(< $PREBUILT_DEPS_FILE)
do
    haddock_html=$(ghc-pkg --simple-output field $pkg haddock-html)

    # Sometimes the referenced html dir file does not exist
    # (e.g. for `nixpkgs.haskellPackages` deps with haddock disabled).
    # In that case, skip this package with a warning.
    if [[ -d "$haddock_html" ]]
    then
        extra_args+=("--local=$haddock_html")
    else
        echo "Warning: haddock missing for package $pkg" 1>&2
    fi
done

echo "$@" "${extra_args[@]}"
$hoogle "$@" "${extra_args[@]}"
