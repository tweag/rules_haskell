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
expected_covered_expressions_percentage={expected_covered_expressions_percentage}
expected_uncovered_expression_count={expected_uncovered_expression_count}
hpc_dir_args=""
mix_file_paths={mix_file_paths}
for m in "${mix_file_paths[@]}"
do
  absolute_mix_file_path=$(rlocation $m)
  hpc_parent_dir=$(dirname $absolute_mix_file_path)
  trimmed_hpc_parent_dir=$(echo "${hpc_parent_dir%%.hpc*}")
  hpc_dir_args="$hpc_dir_args --hpcdir=$trimmed_hpc_parent_dir.hpc"
done
$binary_path "$@"
$hpc_path report $tix_file_path $hpc_dir_args > __hpc_coverage_report
echo "Overall report"
cat __hpc_coverage_report
covered_expression_percentage=$(grep "expressions used" __hpc_coverage_report | cut -c 1-3)
if [ $covered_expression_percentage -lt $expected_covered_expressions_percentage ]
then
  echo -e "\n==>$ERRORCOLOR Inadequate expression coverage percentage.$CLEARCOLOR Expected $expected_covered_expressions_percentage%, but actual coverage was $ERRORCOLOR$(($covered_expression_percentage))%$CLEARCOLOR.\n"
  exit 1
fi
uncovered_expression_count=$(grep "expressions used" __hpc_coverage_report | sed s/.*\(//g | cut -f1 -d "/")
if [ $uncovered_expression_count -gt $expected_uncovered_expression_count ]
then
  echo -e "\n==>$ERRORCOLOR Overly large uncovered expression count.$CLEARCOLOR Expected $expected_uncovered_expression_count uncovered expressions, but actual uncovered expression count was $ERRORCOLOR$(($uncovered_expression_count))%$CLEARCOLOR.\n"
  exit 1
fi
