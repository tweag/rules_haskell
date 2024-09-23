#!/usr/bin/env bash

# Run the start script in its own workspace
# and build the example binary target.

set -e

rules_haskell_dir=$(cd ..; pwd)

# Always run start script in the same directory, for caching.
# See https://docs.bazel.build/versions/master/output_directories.html.
workdir=/tmp/bazel-run-start-script
rm -rf $workdir
mkdir $workdir
cd $workdir
cp "$rules_haskell_dir/.bazelversion" .

# specify version for bazelisk via `USE_BAZEL_VERSION`, since it does not read the .bazelversion when there is no WORKSPACE file
USE_BAZEL_VERSION=$( cat .bazelversion )
export USE_BAZEL_VERSION

# arguments are passed on to the start script
"$rules_haskell_dir/start" "$@"

# Set Nixpkgs in environment variable to avoid hardcoding it in
# start script itself.

# overrides the used rules_haskell, because
# when we're testing the start on a feature branch (CI),
# the latest rules_haskell version doesn't always work.
# If on the branch we update Bazel to a version with breaking
# changes, then we need to adapt to those changes in the branch.
# Which in turn means the start script should pull in those changes too.

NIX_PATH=nixpkgs="$rules_haskell_dir/nixpkgs/default.nix" \
  bazel run \
  --config=ci \
  --override_repository=rules_haskell="$rules_haskell_dir" \
  --override_module=rules_haskell="$rules_haskell_dir" \
  //:example
