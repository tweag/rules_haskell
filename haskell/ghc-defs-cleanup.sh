#!/bin/sh
#
# Usage: ghc-defs-cleanup.sh <GREP_PATH> <INPUT_FILE> <OUTPUT_FILE>

# This is what Cabal sets:
#
# https://github.com/haskell/cabal/blob/master/Cabal/Distribution/Simple/PreProcess.hs#L530-L537
#
# so we should be OK too.

$1 "#define __GLASGOW_HASKELL__" $2 >  $3
$1 "#define .*_HOST_OS"          $2 >> $3
$1 "#define .*_BUILD_ARCH"       $2 >> $3
