#!/usr/bin/env bash
#
# Usage: haddock-wrapper.sh <PREBUILD_DEPS_FILE> <HADDOCK_ARGS>

set -eo pipefail

PREBUILT_DEPS_FILE=$1
shift

extra_args=()

for pkg in $(< $PREBUILT_DEPS_FILE)
do
    haddock_interfaces=$(ghc-pkg --simple-output field $pkg haddock-interfaces)
    haddock_html=$(ghc-pkg --simple-output field $pkg haddock-html)
    extra_args+=("--read-interface=$haddock_html,$haddock_interfaces")
done

haddock "${extra_args[@]}" "$@"
