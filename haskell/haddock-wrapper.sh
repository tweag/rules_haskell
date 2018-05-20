#!/usr/bin/env bash
#
# Usage: haddock-wrapper.sh <PREBUILD_DEPS_FILE> <HADDOCK_ARGS>
#
# Environment variables:
#   * RULES_HASKELL_GHC_PKG -- location of ghc-pkg
#   * RULES_HASKELL_HADDOCK -- location of haddock

set -e
set -o pipefail

PREBUILT_DEPS_FILE=$1
shift

extra_args=()

for pkg in $(< $PREBUILT_DEPS_FILE)
do
    haddock_interfaces=$($RULES_HASKELL_GHC_PKG --simple-output field $pkg haddock-interfaces)
    haddock_html=$($RULES_HASKELL_GHC_PKG --simple-output field $pkg haddock-html)
    extra_args+=("--read-interface=$haddock_html,$haddock_interfaces")
done

"$RULES_HASKELL_HADDOCK" "${extra_args[@]}" "$@"
