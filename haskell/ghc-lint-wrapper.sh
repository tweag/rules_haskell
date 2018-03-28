#!/usr/bin/env bash
#
# Usage: ghc-lint-wrapper.sh <GHC_ARGS>

set -o pipefail # requires bash, not available in standard sh

"$RULES_HASKELL_GHC" "$@" 2>&1 | "$RULES_HASKELL_TEE" "$RULES_HASKELL_LINT_OUTPUT"
