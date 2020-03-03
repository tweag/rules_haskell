#!/bin/sh
#
# Usage:
#
# ./serve-docs.sh [PORT_NUMBER]

set -e

SCRATCH=$(mktemp -d --tmpdir rules_haskell-docs.XXXX)
PORT=${1:-8000}

finish() {
    echo "Deleting $SCRATCH ..."
    rm -rf "$SCRATCH"
}

trap finish EXIT

bazel build //docs:api_html
mkdir "$SCRATCH/api"
unzip -d "$SCRATCH/api" bazel-bin/docs/api_html-stardoc.zip

bazel build //docs:guide_html
mkdir "$SCRATCH/guide"
unzip -d "$SCRATCH/guide" bazel-bin/docs/guide_html.zip

cd "$SCRATCH"
python3 -m http.server "$PORT"
