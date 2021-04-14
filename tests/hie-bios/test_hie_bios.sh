#!/usr/bin/env bash

# To test the hie_bios_path_prefix argument of haskell_repl rule.
# we check that the prefix $magic_string is added to some paths of file $1
# and that if we remove them, we get back file $2

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

FILE1="$(rlocation "$TEST_WORKSPACE/$1")"
FILE2="$(rlocation "$TEST_WORKSPACE/$2")"
if grep -q '$magic_string' "$FILE1"; then
    if sed 's#$magic_string/##' "$FILE1" | diff "$FILE2" -; then
	exit 0
    else
	echo "$2 ="
	od -c "$FILE2"
	echo "$1 ="
	od -c "$FILE1"
	echo "$1 (sed output) ="
	sed 's#$magic_string/##' "$FILE1" | od -c
	exit 1
    fi
else
    echo "\$magic_string should be in file: $1"
    echo "$1 ="
    od -c "$FILE1"
    exit 1
fi
