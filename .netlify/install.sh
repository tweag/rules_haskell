#!/bin/sh
set -eux
INSTALL="$(.ci/fetch-bazel-bindist)"
mv "$INSTALL" "$HOME/bin"
