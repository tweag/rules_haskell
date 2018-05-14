#!/usr/bin/env bash
#
# Usage: copy-dep-haddock.sh
#
# Environment variables:
#   * RULES_HASKELL_MKDIR -- location of mkdir
#   * RULES_HASKELL_CP -- location of cp
#   * RULES_HASKELL_DOC_DIR -- root of doc directory
#   * RULES_HASKELL_HTML_DIR -- html directory with Haddocks to copy
#   * RULES_HASKELL_TARGET_DIR -- directory where to copy contents of html dir

set -o pipefail

# Ensure that top-level doc directory exists.

"$RULES_HASKELL_MKDIR" -p "$RULES_HASKELL_DOC_DIR"

# Copy Haddocks of a dependency.

"$RULES_HASKELL_CP" -r "$RULES_HASKELL_HTML_DIR" "$RULES_HASKELL_TARGET_DIR"
