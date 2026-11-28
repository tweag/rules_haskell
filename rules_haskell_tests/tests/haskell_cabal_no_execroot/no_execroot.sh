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

# Every absolute path into the execroot, sandboxed or not, contains an
# `execroot/<workspace>` component, with either kind of separator on Windows.
# Match `execroot` as a whole component: this package's own name ends in
# `_execroot`, and relative paths into it are fine.
pattern='[^[:space:][:cntrl:]"]*[/\\]execroot[/\\][^[:space:][:cntrl:]"]*'

if [[ $# -eq 0 ]]; then
  echo >&2 "ERROR: no files to check"
  exit 1
fi

status=0
for path in "$@"; do
  file="$(rlocation "$TEST_WORKSPACE/$path")"
  if [[ ! -f "$file" ]]; then
    echo >&2 "ERROR: cannot find $path"
    exit 1
  fi
  if grep -a -i -q -E "$pattern" "$file"; then
    echo >&2 "ERROR: $path contains the absolute execroot:"
    grep -a -i -o -E "$pattern" "$file" | sort -u >&2
    status=1
  fi
done
exit "$status"
