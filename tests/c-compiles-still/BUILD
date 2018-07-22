package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
)

haskell_library(
    name = "foo",
    srcs = ["Foo.hs"],
    deps = [
        "//tests:base",
        "//tests/c-compiles:c-lib",
    ],
)
