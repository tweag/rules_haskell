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

# Derived from Bazel's Bash runfiles library (tools/bash/runfiles/runfiles.bash).
if [[ -z "$RUNFILES_DIR" ]]; then
  if [[ -d "$0.runfiles" ]]; then
    export RUNFILES_DIR="$0.runfiles"
  fi
fi
if [[ -z "$RUNFILES_MANIFEST_FILE" ]]; then
  if [[ -f "$0.runfiles_manifest" ]]; then
    export RUNFILES_MANIFEST_FILE="$0.runfiles_manifest"
  elif [[ -f "$0.runfiles/MANIFEST" ]]; then
    export RUNFILES_MANIFEST_FILE="$0.runfiles/MANIFEST"
  fi
fi

# GHCi script and libraries are loaded relative to workspace directory.
# bazel run //some:target@repl will be executed from the workspace directory.
# bazel run //some:haskell_repl will be executed from its execroot.
# Explicitly change into the workspace root in that case.
cd "$BUILD_WORKSPACE_DIRECTORY"

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
TOOL_LOCATION="$RULES_HASKELL_EXEC_ROOT/{TOOL}"
# Setting -pgm* flags explicitly has the unfortunate side effect
# of resetting any program flags in the GHC settings file. So we
# restore them here. See
# https://ghc.haskell.org/trac/ghc/ticket/7929.
PGM_ARGS="-pgma {CC} -pgmc {CC} -pgml {CC} -pgmP {CC} -optc-fno-stack-protector -optP-E -optP-undef -optP-traditional"

{ENV}
"$TOOL_LOCATION" $PGM_ARGS {ARGS} "$@"
