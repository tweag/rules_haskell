#!/bin/sh
#
# Usage: make-bin-symlink-realpath.sh <TARGET> <SYMLINK>

set -e # fail if any invocation below fails

mkdir -p $(dirname "$2")
ln -s $(realpath "$1") "$2"
