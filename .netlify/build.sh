#!/bin/sh

set -eux

rm -rf public
mkdir public
npx @bazel/bazelisk build --spawn_strategy=local //docs:api_html
unzip -d public bazel-bin/docs/api_html-stardoc.zip
cp start public
