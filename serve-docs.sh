#!/bin/sh
#
# Usage:
#
# ./serve-docs.sh [PORT_NUMBER]

set -e

SCRATCH=$(mktemp -d --tmpdir rules_haskell-docs.XXXX)
PORT=${1:-8000}

function finish {
    echo Deleting $SCRATCH ...
    rm -rf "$scratch"
}

trap finish EXIT

bazel build //haskell:doc_html
mkdir $SCRATCH/api
unzip -d $SCRATCH/api bazel-bin/haskell/doc_html-skydoc.zip

bazel build //docs
mkdir $SCRATCH/docs
unzip -d $SCRATCH/docs bazel-genfiles/docs/docs.zip

cd $SCRATCH
python -m SimpleHTTPServer $PORT
