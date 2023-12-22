#!/bin/sh

set -eux

npx @bazel/bazelisk build //docs:api_html
mkdir -p public
unzip -d public bazel-bin/docs/api_html-stardoc.zip
cp start public
