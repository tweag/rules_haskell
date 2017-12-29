#!/usr/bin/env bash
#
# Usage: ghc-version-check.sh <COMPILER_PATH> <VERSION_FILE> <EXPECTED_VERSION>

$1 --numeric-version > $2
if [[ $3 != $(< $2) ]]
then
    echo GHC version $(< $2) does not match expected version $3.
    exit 1
fi
