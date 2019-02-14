load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_import",
    "haskell_library",
)

haskell_import(name = "base")

haskell_library(
    name = "foo",
    srcs = ["Baz.hs"],
    visibility = ["//visibility:public"],
    deps = ["//tests/hackage:base", "@gmp"],
)
