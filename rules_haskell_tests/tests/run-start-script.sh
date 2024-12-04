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

function getattr_value() {
    local nix_file="$rules_haskell_dir/nixpkgs/default.nix"
    while IFS=$' \t;' read -r key eq value rest ; do
        if [ "$key" == "$1" ] && [ "$eq" == '=' ]; then
            value="${value%\"}"
            value="${value#\"}"

            echo "$value"
            return
        fi
    done < "$nix_file"
    echo "could not lookup ${1} in $nix_file" >&2
    exit 1
}

function have() {
    command -v "$1" &> /dev/null
}

if have nix; then
    NIXPKGS_REVISION=$( getattr_value "rev" )
    # N.B. the sha256 hash attribute given to `builtins.fetchTarball` is computed after unpacking
    #      the archive, it is not the hash of the downloaded artifact
    #NIXPKGS_HASH=$( nix hash to-sri "$(getattr_value "sha256")" )
fi

export NIXPKGS_REVISION NIXPKGS_HASH

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
