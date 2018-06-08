#!/bin/sh

# Any function named "test_*" will be run as a test

test_bazel_test()
{
    bazel test //... --config=ci
}


test_failures() {
    # Test targets that must fail
    for i in $(bazel query 'kind(rule, //tests/failures/...) intersect attr("tags", "manual", //tests/failures/...)')
    do
        assertBuildFailure "$i"
    done
}

# Test REPL for libraries
test_repl_libraries() {
    bazel build //tests/repl-targets:hs-lib-repl
    bazel-bin/tests/repl-targets/hs-lib-repl -e "foo 10"
}

# Test REPL for binaries
test_repl_binaries() {
    bazel build //tests/repl-targets:hs-bin-repl
    bazel-bin/tests/repl-targets/hs-bin-repl -e ":main"
}


# Test start script
test_startup_script() {
    pwd=$(pwd)
    cd $(mktemp -d)
    $pwd/start

    # Copy the bazel configuration, this is only useful for CI
    mkdir tools
    cp $pwd/tools/bazel.rc tools/bazel.rc

    bazel fetch //... --config=ci
}
