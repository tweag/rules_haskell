load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_import",
    "haskell_library",
)

haskell_import(name = "base")

haskell_library(
    name = "foo",
    srcs = ["Foo.hs"],
    deps = [":base"],
    visibility = ["//visibility:public"],
)
