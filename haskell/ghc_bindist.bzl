"""Workspace rules (GHC binary distributions)

Loading from this file is **deprecated**. Load from
`@rules_haskell//haskell/toolchains:ghc_bindist.bzl` instead.
"""

load(
    "//haskell/toolchains:ghc_bindist.bzl",
    _haskell_register_ghc_bindists = "haskell_register_ghc_bindists",
    _ghc_bindist = "ghc_bindist",
)

def ghc_bindist(**kwargs):
    print("DEPRECATED: import from @rules_haskell/haskell/toolchains:ghc_bindist.bzl instead.")
    _ghc_bindist(**kwargs)

def haskell_register_ghc_bindists(**kwargs):
    print("DEPRECATED: import from @rules_haskell/haskell/toolchains:ghc_bindist.bzl instead.")
    _ghc_bindist(**kwargs)
