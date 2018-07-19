#!/bin/sh

set -e

# Include libdir in include path just like hsc2hs does.
libdir=$(ghc --print-libdir)
c2hs -C-I$libdir/include ${1+"$@"}
