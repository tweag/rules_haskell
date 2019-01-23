"""
    Fetches GHC boot packages from GHC directly rather than from Nixpkgs
    for better bindist support.
"""

package(default_visibility = ["//tests:__subpackages__"])

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_import",
)

[
    haskell_import(name = name)
    for name in [
        "base",
        "binary",
        "bytestring",
        "containers",
        "directory",
        "filepath",
        "template-haskell",
        "ghc-prim",
        "text",
    ]
]
