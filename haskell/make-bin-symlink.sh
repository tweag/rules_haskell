#!/bin/sh
#
# Usage: make-bin-symlink.sh <MKDIR_LOCATION> <REALPATH_LOCATION>
#                            <LN_LOCATION> <TARGET> <SYMLINK_DIR> <SYMLINK>

set -e # fail if any invocation below fails

ABS_TARGET=$("$2" "$4")

"$1" -p "$5"
"$3" -s "$ABS_TARGET" "$6"
