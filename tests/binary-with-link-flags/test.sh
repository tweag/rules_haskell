#/usr/bin/env sh

set -e

# Fails if executable was linked without -threaded flags.
binary-with-link-flags +RTS -N
