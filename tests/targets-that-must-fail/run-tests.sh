#!/bin/sh

set -e

test_transitive_deps() {
    set +e

    bazel build @targets_that_must_fail//transitive-deps:lib-c
    if [ $? -eq 0 ]; then
        echo "bazel build @targets_that_must_fail//transitive-deps:lib-c should have failed."
        exit 1
    fi

    bazel build @targets_that_must_fail//transitive-deps:lib-d
    if [ $? -eq 0 ]; then
        echo "bazel build @targets_that_must_fail//transitive-deps:lib-d should have failed."
        exit 1
    fi

    set -e
    exit 0
}

test_transitive_deps
