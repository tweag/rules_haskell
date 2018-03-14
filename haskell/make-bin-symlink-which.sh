#!/bin/sh
#
# Usage: make-bin-symlink-which.sh <TARGET> <SYMLINK>

set -e # fail if any invocation below fails

mkdir -p $(dirname "$2")
ln -s $(which "$1") "$2"
