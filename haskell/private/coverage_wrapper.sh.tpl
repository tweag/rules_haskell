#!/usr/bin/env bash
# A wrapper for Haskell binaries which have been instrumented for hpc code coverage.

# Copy-pasted from Bazel's Bash runfiles library (tools/bash/runfiles/runfiles.bash).
set -euo pipefail
if [[ ! -d "${RUNFILES_DIR:-/dev/null}" && ! -f "${RUNFILES_MANIFEST_FILE:-/dev/null}" ]]; then
  if [[ -f "$0.runfiles_manifest" ]]; then
    export RUNFILES_MANIFEST_FILE="$0.runfiles_manifest"
  elif [[ -f "$0.runfiles/MANIFEST" ]]; then
    export RUNFILES_MANIFEST_FILE="$0.runfiles/MANIFEST"
  elif [[ -f "$0.runfiles/bazel_tools/tools/bash/runfiles/runfiles.bash" ]]; then
    export RUNFILES_DIR="$0.runfiles"
  fi
fi
if [[ -f "${RUNFILES_DIR:-/dev/null}/bazel_tools/tools/bash/runfiles/runfiles.bash" ]]; then
  source "${RUNFILES_DIR}/bazel_tools/tools/bash/runfiles/runfiles.bash"
elif [[ -f "${RUNFILES_MANIFEST_FILE:-/dev/null}" ]]; then
  source "$(grep -m1 "^bazel_tools/tools/bash/runfiles/runfiles.bash " \
            "$RUNFILES_MANIFEST_FILE" | cut -d ' ' -f 2-)"
else
  echo >&2 "ERROR: cannot find @bazel_tools//tools/bash/runfiles:runfiles.bash"
  exit 1
fi
# --- end runfiles.bash initialization ---

ERRORCOLOR='\033[1;31m'
CLEARCOLOR='\033[0m'
binary_path=$(rlocation {binary_path})
hpc_path=$(rlocation {hpc_path})
tix_file_path={tix_file_path}
expected_expression_coverage={expected_expression_coverage}
hpc_dir_args=""
for m in {mix_file_paths}
do
  absolute_mix_file_path=$(rlocation $m)
  hpc_dir_args="$hpc_dir_args --hpcdir=$(dirname $absolute_mix_file_path)"
done
$binary_path "$@"
$hpc_path report $tix_file_path $hpc_dir_args > __hpc_coverage_report
echo "Overall report"
cat __hpc_coverage_report
expression_coverage=$(grep "expressions used" __hpc_coverage_report | cut -c 1-3)
if [ $expression_coverage -lt $expected_expression_coverage ]
then
  echo -e "\n==>$ERRORCOLOR Inadequate expression coverage.$CLEARCOLOR Expected $expected_expression_coverage%, but actual coverage was $ERRORCOLOR$(($expression_coverage))%$CLEARCOLOR.\n"
  exit 1
fi
