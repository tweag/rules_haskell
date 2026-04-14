#!/bin/sh

set -eux

rm -rf public
mkdir public
export USE_BAZEL_VERSION=7.x
npx @bazel/bazelisk build //docs:api_html
unzip -d public bazel-bin/docs/api_html-stardoc.zip
cp start public
