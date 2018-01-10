#/usr/bin/env sh

set -e

# Fails if executable was linked without -threaded flag.
$1 +RTS -N
