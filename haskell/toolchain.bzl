"""Haskell toolchain utilities"""

load(
    ":private/toolchain/nixpkgs.bzl",
    _haskell_nixpkgs_patched_solib = "haskell_nixpkgs_patched_solib",
)

haskell_nixpkgs_patched_solib = _haskell_nixpkgs_patched_solib
