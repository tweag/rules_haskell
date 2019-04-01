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

# either of the two expected coverage metrics should be set to -1 if they're meant to be unused
expected_covered_expressions_percentage={expected_covered_expressions_percentage}
expected_uncovered_expression_count={expected_uncovered_expression_count}

strict_coverage_analysis={strict_coverage_analysis}
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

if [ "$expected_covered_expressions_percentage" -ne -1 ]
then
  covered_expression_percentage=$(grep "expressions used" __hpc_coverage_report | cut -c 1-3)
  if [ "$covered_expression_percentage" -lt "$expected_covered_expressions_percentage" ]
  then
    echo -e "\n==>$ERRORCOLOR Inadequate expression coverage percentage.$CLEARCOLOR"
    echo -e "==> Expected $expected_covered_expressions_percentage%, but the actual coverage was $ERRORCOLOR$(($covered_expression_percentage))%$CLEARCOLOR.\n"
    exit 1
  elif [ "$strict_coverage_analysis" == "True" ] && [ "$covered_expression_percentage" -gt "$expected_covered_expressions_percentage" ]
  then
    echo -e "\n==>$ERRORCOLOR ** BECAUSE STRICT COVERAGE ANALYSIS IS ENABLED **$CLEARCOLOR"
    echo -e "==> Your coverage percentage is now higher than expected.$CLEARCOLOR"
    echo -e "==> Expected $expected_covered_expressions_percentage% of expressions covered, but the actual value is $ERRORCOLOR$(($covered_expression_percentage))%$CLEARCOLOR."
    echo -e "==> Please increase the expected coverage percentage to match.\n"
    exit 1
  fi
fi

if [ "$expected_uncovered_expression_count" -ne -1 ]
then
  coverage_numerator=$(grep "expressions used" __hpc_coverage_report | sed s:.*\(::g | cut -f1 -d "/")
  coverage_denominator=$(grep "expressions used" __hpc_coverage_report | sed s:.*/::g | cut -f1 -d ")")
  uncovered_expression_count="$(($coverage_denominator - $coverage_numerator))"
  if [ "$uncovered_expression_count" -gt "$expected_uncovered_expression_count" ]
  then
    echo -e "\n==>$ERRORCOLOR Too many uncovered expressions.$CLEARCOLOR"
    echo -e "==> Expected $expected_uncovered_expression_count uncovered expressions, but the actual count was $ERRORCOLOR$(($uncovered_expression_count))$CLEARCOLOR.\n"
    exit 1
  elif [ "$strict_coverage_analysis" == "True" ] && [ "$uncovered_expression_count" -lt "$expected_uncovered_expression_count" ]
  then
    echo -e "\n==>$ERRORCOLOR ** BECAUSE STRICT COVERAGE ANALYSIS IS ENABLED **$CLEARCOLOR"
    echo -e "==>$ERRORCOLOR Your uncovered expression count is now lower than expected.$CLEARCOLOR"
    echo -e "==> Expected $expected_uncovered_expression_count uncovered expressions, but there is $ERRORCOLOR$(($uncovered_expression_count))$CLEARCOLOR."
    echo -e "==> Please lower the expected uncovered expression count to match.\n"
    exit 1
  fi
fi