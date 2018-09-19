#!/bin/sh

set -eux

V=0.16.0

curl -LO https://github.com/bazelbuild/bazel/releases/download/$V/bazel-$V-installer-linux-x86_64.sh
chmod +x bazel-$V-installer-linux-x86_64.sh
./bazel-$V-installer-linux-x86_64.sh --user

# XXX: Hack to prevent the `gen_packages_list` rule from crashing
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
