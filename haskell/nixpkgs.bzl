"""Workspace rules (Nixpkgs)

Loading from this file is **deprecated**. Load from
`@rules_haskell//haskell/toolchains:nixpkgs.bzl` instead.
"""

load(
    "//haskell/toolchains:nixpkgs.bzl",
    _haskell_register_ghc_nixpkgs = "haskell_register_ghc_nixpkgs",
)

def haskell_register_ghc_nixpkgs(**kwargs):
    print("DEPRECATED: import from @rules_haskell/haskell/toolchains:nixpkgs.bzl instead.")
    _haskell_register_ghc_nixpkgs(**kwargs)
