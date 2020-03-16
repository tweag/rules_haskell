#!/bin/sh

set -eux

export PATH=$HOME/bin:$PATH

# XXX We don't want to be using the Nixpkgs CC toolchain, because
# Nixpkgs is not available. But currently we can only override the
# autoconfigured CC toolchain, not have several (which we would then
# select via --extra_toolchains). So here's a gross hack that simply
# patches out the nixpkgs_cc_configure() line.
#
# See https://github.com/bazelbuild/bazel/issues/6696.
awk '
  BEGIN {del=0}
  /^nixpkgs_cc_configure\(/ {del=1}
  del==0 {print}
  /\)/ {del=0}' WORKSPACE > WORKSPACE.tmp
  # Note: awk -i inplace not available
mv WORKSPACE.tmp WORKSPACE

# We don't want to be depending on Nixpkgs for documentation
# generation either.
sed -i 's/vendored_node = "@nixpkgs_nodejs"/vendored_node = None/' WORKSPACE

bazel build //docs:api_html
mkdir -p public
unzip -d public bazel-bin/docs/api_html-stardoc.zip
cp start public
