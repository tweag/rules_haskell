#!/bin/sh

set -eux

export PATH=$HOME/bin:$PATH

bazel build //docs
unzip -d public bazel-bin/docs/docs-skydoc.zip
cp start public
