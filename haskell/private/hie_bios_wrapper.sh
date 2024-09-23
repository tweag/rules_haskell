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

# shellcheck disable=SC1083
SCRIPT_REL=%{OUTPUT}
SCRIPT_ABS="$(rlocation %{OUTPUT_RLOCATION_PATH})"
# shellcheck disable=SC2034
RULES_HASKELL_EXEC_ROOT=${SCRIPT_ABS%/"$SCRIPT_REL"}
ARGS="%{ARGS}"
echo "$ARGS"
UNIT_FILE_FRAGMENTS="%{UNIT_FILE_FRAGMENTS}"
for FRAGMENT in $UNIT_FILE_FRAGMENTS; do
  UNIT_FILE=${FRAGMENT%.sh}
  . "$FRAGMENT" >"$UNIT_FILE"
  echo "-unit"
  echo "@${UNIT_FILE}"
done
