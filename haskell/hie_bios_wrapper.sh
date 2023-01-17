#!/usr/bin/env bash

# --- begin runfiles.bash initialization v2 ---
# Copy-pasted from the Bazel Bash runfiles library v2.
set -uo pipefail; set +e; f=bazel_tools/tools/bash/runfiles/runfiles.bash
source "${RUNFILES_DIR:-/dev/null}/$f" 2>/dev/null || \
  source "$(grep -sm1 "^$f " "${RUNFILES_MANIFEST_FILE:-/dev/null}" | cut -f2- -d' ')" 2>/dev/null || \
  source "$0.runfiles/$f" 2>/dev/null || \
  source "$(grep -sm1 "^$f " "$0.runfiles_manifest" | cut -f2- -d' ')" 2>/dev/null || \
  source "$(grep -sm1 "^$f " "$0.exe.runfiles_manifest" | cut -f2- -d' ')" 2>/dev/null || \
  { echo>&2 "ERROR: cannot find $f"; exit 1; }; f=; set -e
# --- end runfiles.bash initialization v2 ---

echo "start"

ARGS_RAW='%{ARGS}'
SCRIPT_DIR_REL='%{CURRENT_TARGET_DIR}'
SCRIPT_DIR_ABS="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 || exit ; pwd -P )"
RULES_HASKELL_EXEC_ROOT=${SCRIPT_DIR_ABS%"/$SCRIPT_DIR_REL"}
echo "RULES_HASKELL_EXEC_ROOT=$RULES_HASKELL_EXEC_ROOT"

echo "PWD=$(pwd)"
echo "SCRIPT_DIR_ABS=$SCRIPT_DIR_ABS"

echo $ARGS_RAW

ARGS="%{ARGS}"
echo "$ARGS"
