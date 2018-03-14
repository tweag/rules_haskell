#!/usr/bin/env bash
#
# Usage: ghci-repl-wrapper.sh <ARGS>

# First test if ghci can be found where we expect it to be. If not, it's
# likely that the script is invoked incorrectly, so we need to output a
# warning with instructions how to invoke it.

GHCI_LOCATION="{GHCi}"
SCRIPT_LOCATION="{SCRIPT_LOCATION}"

if ! [ -e "$GHCI_LOCATION" ]
then
    cat <<EOF
It looks like you are trying to invoke the REPL incorrectly.
Due to limitations in Bazel, "bazel run" should not be used
for that (because it closes stdin).

Instead please execute the following from workspace root:

$ $SCRIPT_LOCATION
EOF
    exit 1
fi

export LD_LIBRARY_PATH={LDLIBPATH}
"$GHCI_LOCATION" {ARGS} "$@"
