ghc_version = "8.6.4"

# XXX: We must currently keep a different GHC version for bindists
# because 8.6.3 fails on Windows with Template Haskell.
#
# See: https://ghc.haskell.org/trac/ghc/ticket/16057 for details.

bindists_ghc_version = "8.6.2"
