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


# Load the file which contains the tests
source ./all_tests.sh

if [ "$#" -eq 0 ]; then
    # Auto detect tests starting with "test_" and run them
    TESTS=$(grep -o '^test_[_[:alnum:]]\+' all_tests.sh)
else
    # runs test specified on the command line
    TESTS=$@
fi

for i in $TESTS
do
    run_test $i
done
