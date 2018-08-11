#!/bin/sh

set -eux

export PATH=$HOME/bin:$PATH

# Host configuration has optimizations turned on by default. Turn them
# off to save time.
bazel build --host_copt=-O0 //docs
unzip -d public bazel-bin/docs/docs-skydoc.zip
cp start public
