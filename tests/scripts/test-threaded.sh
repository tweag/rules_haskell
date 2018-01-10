#/usr/bin/env sh

set -e

# Fails if executable was linked without -threaded flags.
$1 +RTS -N
