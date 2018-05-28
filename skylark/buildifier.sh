#!/usr/bin/env bash

buildifier=$(pwd)/$1
shift

function die() {
  echo "$1"
  exit 1
}

if [[ -n "$BUILD_WORKING_DIRECTORY" ]]
then
    die "Error: script must be called using bazel run --direct_run."
fi

cd $BUILD_WORKING_DIRECTORY
$buildifier $@
