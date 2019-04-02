#!/bin/sh

# Run the start script in its own workspace
# and build the example binary target.

set -e

pwd=$(pwd)
cd $(mktemp -d)
$pwd/start

# Copy the bazel configuration, this is only useful for CI
mkdir tools
cp $pwd/.bazelrc .bazelrc

# Set Nixpkgs in environment variable to avoid hardcoding it in
# start script itself
NIX_PATH=nixpkgs=$pwd/nixpkgs/default.nix \
  bazel build \
  --config=ci \
  //:example
