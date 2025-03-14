#!/usr/bin/env bash
#
# Usage: indexer-wrapper.sh <GHC_ARGS>

set -eo pipefail

%{env}

# BSD and GNU mktemp are very different; attempt GNU first
TEMP=$(mktemp -d 2>/dev/null || mktemp -d -t 'indexer_wrapper')
trap cleanup 1 2 3 6
cleanup() { rm -rf "$TEMP"; }

/usr/bin/tree > %{logfile}

libdir=$($GHC --print-libdir)
TMPDIR=$TEMP %{indexer} -c $CORPUS -B $libdir -- "$@" > %{outfile} 2>> %{logfile}


cleanup
