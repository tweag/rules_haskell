#!/usr/bin/env bash
#
# Usage: ghci_repl_wrapper.sh <ARGS>

# this variable is set by `bazel run`
if [ "$BUILD_WORKSPACE_DIRECTORY" = "" ]
then
    cat <<EOF
It looks like you are trying to invoke the REPL incorrectly.
We only support calling the repl script with

$ bazel run <target>

for now.

If you are on bazel < 0.15 you must invoke as follows:

$ bazel run --direct_run <target>
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


RULES_HASKELL_EXEC_ROOT=$(dirname $(readlink ${BUILD_WORKSPACE_DIRECTORY}/bazel-out))
GHCI_LOCATION="$RULES_HASKELL_EXEC_ROOT/{GHCi}"
SCRIPT_LOCATION="$RULES_HASKELL_EXEC_ROOT/{SCRIPT_LOCATION}"


export LD_LIBRARY_PATH={LDLIBPATH}
"$GHCI_LOCATION" {ARGS} "$@"
