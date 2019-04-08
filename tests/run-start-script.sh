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
# start script itself.

# overrides the used rules_haskell, because
# when we're testing the start on a feature branch (CI),
# the latest rules_haskell version doesn't always work.
# If on the branch we update Bazel to a version with breaking
# changes, then we need to adapt to those changes in the branch.
# Which in turn means the start script should pull in those changes too.

NIX_PATH=nixpkgs=$pwd/nixpkgs/default.nix \
  bazel build \
  --config=ci \
  --override_repository=io_tweag_rules_haskell=$pwd \
  //:example
