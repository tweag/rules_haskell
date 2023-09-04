#!/usr/bin/env bash

if [ "$BUILD_WORKSPACE_DIRECTORY" = "" ]; then
    cat <<EOF >&2
It looks like you are trying to invoke the pin script incorrectly.
We only support calling the pin script with

    bazel run @STACKAGE-unpinned//:pin

EOF
    exit 1
fi
cd "$BUILD_WORKSPACE_DIRECTORY"

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

cp "$(rlocation "{repository_name}-unpinned/{stack_snapshot_source}")" "{stack_snapshot_location}"

if [ "{predefined_stack_snapshot}" = "True" ]; then
    cat <<EOF >&2
Successfully pinned resolved artifacts for @{repository_name}, {stack_snapshot_location} is now up-to-date.
EOF
else
    cat <<EOF >&2
Successfully pinned resolved artifacts for @{repository_name} in {stack_snapshot_location}.
This file should be checked in your version control system.

Next, please update your WORKSPACE file by adding the stack_snapshot_json attribute.
For example:

=============================================================

stack_snapshot(
    packages = ...,
    snapshot = ...,
    stack_snapshot_json = "//:{repository_name}_snapshot.json",
)

=============================================================

To update {repository_name}_snapshot.json, run this command to re-pin the unpinned repository:

    bazel run @{repository_name}-unpinned//:pin

EOF
fi
