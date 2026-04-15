#!/bin/sh

set -eux

rm -rf public
mkdir public
export USE_BAZEL_VERSION=8.x
npx @bazel/bazelisk build --enable_workspace //docs:api_html
unzip -d public bazel-bin/docs/api_html-stardoc.zip
cp start public
