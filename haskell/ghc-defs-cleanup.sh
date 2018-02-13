#!/usr/bin/env bash
#
# Usage: ghc-defs-cleanup.SH <ORIGINAL-FILE> <OUTPUT-FILE>

# This is what Cabal sets:
#
# https://github.com/haskell/cabal/blob/master/Cabal/Distribution/Simple/PreProcess.hs#L530-L537
#
# so we should be OK too.

grep "#define __GLASGOW_HASKELL__" $1 >  $2
grep "#define .*_HOST_OS"          $1 >> $2
grep "#define .*_BUILD_ARCH"       $1 >> $2
