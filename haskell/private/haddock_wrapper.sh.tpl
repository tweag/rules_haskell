#!/usr/bin/env bash
#
# Usage: haddock-wrapper.sh <HADDOCK_ARGS>

set -eo pipefail

# shellcheck disable=SC1083
%{env}

# BSD and GNU mktemp are very different; attempt GNU first
TEMP=$(mktemp -d 2>/dev/null || mktemp -d -t 'haddock_wrapper')
trap cleanup 1 2 3 6
cleanup() { rm -rf "$TEMP"; }
# XXX Override TMPDIR to prevent race conditions on certain platforms.
# This is a workaround for
# https://github.com/haskell/haddock/issues/894.
# shellcheck disable=SC1083
TMPDIR=$TEMP %{haddock} "$@"
cleanup
