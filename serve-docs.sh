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

bazel build //docs:api_html
mkdir $SCRATCH/api
unzip -d $SCRATCH/api bazel-bin/docs/api_html-skydoc.zip

bazel build //docs:guide_html
mkdir $SCRATCH/guide
unzip -d $SCRATCH/guide bazel-genfiles/docs/guide_html.zip

cd $SCRATCH
python -m SimpleHTTPServer $PORT
