#!/usr/bin/env bash
# --- begin runfiles.bash initialization v2 ---
# Copy-pasted from the Bazel Bash runfiles library v2.
set -uo pipefail; f=bazel_tools/tools/bash/runfiles/runfiles.bash
source "${RUNFILES_DIR:-/dev/null}/$f" 2>/dev/null || \
  source "$(grep -sm1 "^$f " "${RUNFILES_MANIFEST_FILE:-/dev/null}" | cut -f2- -d' ')" 2>/dev/null || \
  source "$0.runfiles/$f" 2>/dev/null || \
  source "$(grep -sm1 "^$f " "$0.runfiles_manifest" | cut -f2- -d' ')" 2>/dev/null || \
  source "$(grep -sm1 "^$f " "$0.exe.runfiles_manifest" | cut -f2- -d' ')" 2>/dev/null || \
  { echo>&2 "ERROR: cannot find $f"; exit 1; }; f=; set -e
# --- end runfiles.bash initialization v2 ---
set -euo pipefail

# Construct a new runfiles tree for the test runner in the test sandbox.
RUNNER="$(rlocation "$TEST_WORKSPACE/$1")"
NEW_RUNFILES_DIR="${RUNNER}.runfiles/$TEST_WORKSPACE"
cleanup() { rm -rf "$NEW_RUNFILES_DIR"; }
trap cleanup EXIT
mkdir -p "$NEW_RUNFILES_DIR"
echo "baz" >"$NEW_RUNFILES_DIR/baz.txt"

# Test whether the runner finds a runfile in the new runfiles tree based on
# `argv[0]` rather than `$RUNFILES_DIR` lookup.
unset RUNFILES_DIR
unset RUNFILES_MANIFEST_FILE
"$RUNNER" baz baz.txt
