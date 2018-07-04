#!/usr/bin/env bash
#
# Usage: ghci_repl_wrapper.sh <ARGS>

# First test if ghci can be found where we expect it to be. If not, it's
# likely that the script is invoked incorrectly, so we need to output a
# warning with instructions how to invoke it.

if [ ! -d "bazel-out" ]; then
    cat <<EOF
The bazel-out symlink must be present in workspace directory.

If you have built your project using a custom symlink prefix
(with the --symlink_prefix option) and bazel-out was not created,
it may mean that you're using a version of Bazel that is not yet
supported by rules_haskell.
EOF
    exit 1
fi

# This is a workaround for https://github.com/bazelbuild/bazel/issues/5506
# and also for the fact that REPL script relies on so-called “convenience
# links” and the names of those links are controlled by the --symlink_prefix
# option, which can be set by the user to something unpredictable.
#
# It seems that we can't locate the files of interest/build outputs in
# general. However, due to “internal issues” in Bazel mentioned e.g.
# https://github.com/bazelbuild/bazel/issues/3796, the directory bazel-out
# is always created under the workspace directory. We exploit this to get
# location of exec root reliably and then prefix locations of various
# components, such as shared libraries with that exec root.

RULES_HASKELL_EXEC_ROOT=$(dirname $(readlink bazel-out))
GHCI_LOCATION="$RULES_HASKELL_EXEC_ROOT/{GHCi}"
SCRIPT_LOCATION="$RULES_HASKELL_EXEC_ROOT/{SCRIPT_LOCATION}"

if ! [ -e "$GHCI_LOCATION" ]
then
    cat <<EOF
It looks like you are trying to invoke the REPL incorrectly.
Due to limitations in Bazel, "bazel run" should not be used
for that (because it closes stdin), although the newer
--direct-run option (available since Bazel 0.12) lifts that
limitation.

Instead please execute the following from workspace root:

$ $SCRIPT_LOCATION
EOF
    exit 1
fi

export LD_LIBRARY_PATH={LDLIBPATH}
"$GHCI_LOCATION" {ARGS} "$@"
