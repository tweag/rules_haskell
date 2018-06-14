load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_toolchain",
  "haskell_doctest_toolchain",
)

exports_files([
    "hazel.bzl",
    "BUILD.ghc",
    "paths-template.hs",
])

haskell_doctest_toolchain(
    name = "doctest",
    doctest = "@haskell_doctest//:doctest_bin",
)
