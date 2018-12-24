#!/bin/sh

set -eux

V=0.20.0

curl -LO https://github.com/bazelbuild/bazel/releases/download/$V/bazel-$V-installer-linux-x86_64.sh
chmod +x bazel-$V-installer-linux-x86_64.sh
./bazel-$V-installer-linux-x86_64.sh --user

# XXX: Hack to prevent the `haskell_nixpkgs_package_list` rule from crashing:
# This rule expects a `nix-build` executable which is used to generate a
# store-path containing an `all-haskell-packages.bzl` file which defines the
# `package` list. Since actually installing `nix-build` on the netlify image
# seems difficult, we provide a dummy shell script which does exactly that.
packages_list=$(mktemp -d)
cat <<EOF > $packages_list/all-haskell-packages.bzl
packages = []
EOF

mkdir -p $HOME
cat <<EOF > $HOME/bin/nix-build
#!/usr/bin/env bash

echo $packages_list
EOF

chmod +x $HOME/bin/nix-build
