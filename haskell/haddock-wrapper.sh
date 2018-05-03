#!/usr/bin/env bash
#
# Usage: haddock-wrapper.sh <HADDOCK_ARGS>
#
# Environment variables:
#   * RULES_HASKELL_GHC_PKG -- location of ghc-pkg
#   * RULES_HASKELL_HADDOCK -- location of haddock
#   * RULES_HASKELL_PREBUILT_DEPS -- pre-built dependencies

set -o pipefail

extra_args=()

for pkg in $RULES_HASKELL_PREBUILT_DEPS
do
    haddock_interfaces=$($RULES_HASKELL_GHC_PKG --simple-output field $pkg haddock-interfaces)
    haddock_html=$($RULES_HASKELL_GHC_PKG --simple-output field $pkg haddock-html)
    extra_args+=("--read-interface=$haddock_html,$haddock_interfaces")
done

"$RULES_HASKELL_HADDOCK" "${extra_args[@]}" "$@"
