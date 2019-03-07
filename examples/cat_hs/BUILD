package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_doc",
  "haskell_toolchain",
)

haskell_toolchain(
  name = "ghc",
  version = "8.4.4",
  tools = "@ghc//:bin",
)

haskell_doc(
  name = "api-doc",
  deps = [
    '//lib/args',
    '//lib/cat',
    '//exec/cat_hs',
  ],
)
