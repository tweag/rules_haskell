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
cd "$BUILD_WORKSPACE_DIRECTORY" || { echo "Cannot cd into $BUILD_WORKSPACE_DIRECTORY"; exit 1; }

# This is a workaround for https://github.com/bazelbuild/bazel/issues/5506
# We recover the execution root by removing the relative path to this script from the absolute one.
# shellcheck disable=SC1083
SCRIPT_DIR_REL=%{OUTPUT}
SCRIPT_DIR_ABS="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 || exit ; pwd -P )"
RULES_HASKELL_EXEC_ROOT=${SCRIPT_DIR_ABS%"$SCRIPT_DIR_REL"}
TOOL_LOCATION="$RULES_HASKELL_EXEC_ROOT/%{TOOL}"
# shellcheck disable=SC1083
ARGS=%{ARGS}

# shellcheck disable=SC1083,SC2288
%{ENV}
"$TOOL_LOCATION" "${ARGS[@]}" "$@"
