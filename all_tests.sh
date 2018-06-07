#!/bin/sh
set -e

# Bazel tests
bazel test //... --config=ci

# Test targets that must fail
tests/targets-that-must-fail/run-tests.sh

# Test REPL for libraries
bazel build //tests/repl-targets:hs-lib-repl
bazel-bin/tests/repl-targets/hs-lib-repl -e "foo 10"

# Test REPL for binaries
bazel build //tests/repl-targets:hs-bin-repl
bazel-bin/tests/repl-targets/hs-bin-repl -e ":main"

# Test start script
(pwd=$(pwd)
cd $(mktemp -d)
$pwd/start

# Copy the bazel configuration, this is only useful for CI
mkdir tools
cp $pwd/tools/bazel.rc tools/bazel.rc

bazel fetch //... --config=ci
)
