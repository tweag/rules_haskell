#!/bin/sh

set -eux

export PATH=$HOME/bin:$PATH

bazel build //haskell:doc_html
unzip -d public bazel-bin/haskell/doc_html-skydoc.zip
cp start public
