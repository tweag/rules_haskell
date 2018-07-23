#!/bin/sh

# This file detects and run all tests
# USAGE:
#
# * Run all tests:
# $ ./run_tests.sh
#
# * Run some tests.sh:
# $ ./run_tests.sh test_bar test_foo

assertBuildFailure() {
    set +e

    bazel build "$1"
    if [ $? -eq 0 ]; then
        echo "bazel build {$1} should have failed."
        exit 1
    fi

    set -e
}

# Color if we are in a terminal, no color otherwise
if [ -t 1 ]
then
    NC='\033[0m'
    GREEN='\033[0;32m'
    RED='\033[0;31m'
fi

run_test() {
  set +e
  SECONDS=0
  TEST_ARG=$@
  echo "running test $TEST_ARG"
  RES=$($TEST_ARG 2>&1)
  RESPONSE_CODE=$?
  DURATION=$SECONDS
  if [ $RESPONSE_CODE -eq 0 ]; then
    echo -e "${GREEN} Test \"$TEST_ARG\" successful ($DURATION sec) $NC"
  else
    echo -e "\nLog:\n"
    echo "$RES"
    echo -e "${RED} Test \"$TEST_ARG\" failed $NC ($DURATION sec) $NC"
    exit $RESPONSE_CODE
  fi
}

############################################################################
# Tests

test_skylark_lint()
{
    bazel test //... --config=ci --build_tests_only --test_tag_filters=lint
}

test_bazel_test()
{
    bazel test //... --config=ci
}

test_bazel_test_prof()
{
    bazel test -c dbg //... --config=ci
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
    # Test whether building of repl forces all runtime dependencies by
    # itself:
    bazel clean
    bazel run --config=ci //tests/repl-targets:hs-lib-repl -- -e "show (foo 10) ++ bar ++ baz ++ gen"
}

# Test REPL for binaries
test_repl_binaries() {
    # Test whether building of repl forces all runtime dependencies by
    # itself:
    bazel clean
    bazel run --config=ci //tests/repl-targets:hs-bin-repl -- -e ":main"
}

# Test `compiler_flags` from toolchain and rule for REPL
test_repl_compiler_flags() {
    # `compiler_flags` from toolchain are correctly used
    bazel run --config=ci //tests/repl-flags:compiler_flags-repl -- -e ":main"
}

# Test `repl_ghci_args` from toolchain and rule for REPL
test_repl_flags() {
    # `compiler_flags` from toolchain are correctly used
    bazel run --config=ci //tests/repl-flags:repl_flags-repl -- -e "foo"
}

# Test start script
test_startup_script() {
    pwd=$(pwd)
    cd $(mktemp -d)
    $pwd/start

    # Copy the bazel configuration, this is only useful for CI
    mkdir tools
    cp $pwd/tools/bazel.rc tools/bazel.rc

    # Set Nixpkgs in environment variable to avoid hardcoding it in
    # start script itself.
    NIX_PATH=nixpkgs=$pwd/nixpkgs.nix bazel fetch //... --config=ci
}

if [ "$#" -eq 0 ]; then
    # Auto detect tests starting with "test_" and run them
    TESTS=$(grep -o '^test_[_[:alnum:]]\+' run_tests.sh)
else
    # runs test specified on the command line
    TESTS=$@
fi

for i in $TESTS
do
    run_test $i
done
