#!/bin/sh

set -eux

export PATH=$HOME/bin:$PATH

bazel build //docs:api_html
unzip -d public bazel-bin/docs/api_html-skydoc.zip
cp start public
