#!/bin/sh

## Tools
assertBuildFailure() {
    set +e

    bazel build "$1"
    if [ $? -eq 0 ]; then
        echo "bazel build {$1} should have failed."
        exit 1
    fi

    set -e
}

## Setup
set -e

## Tests

# Bazel tests
bazel test //... --config=ci

# Test targets that must fail
for i in $(bazel query 'kind(rule, //tests/failures/...) intersect attr("tags", "manual", //tests/failures/...)')
do
   assertBuildFailure "$i"
done

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
