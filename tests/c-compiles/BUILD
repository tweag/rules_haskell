package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_binary",
    "haskell_library",
)

haskell_library(
    name = "hs-lib",
    srcs = ["Lib.hs"],
    deps = [
        "//tests:base",
        "//tests/data:ourclibrary",
    ],
)

haskell_binary(
    name = "c-compiles",
    srcs = ["Main.hs"],
    visibility = ["//visibility:public"],
    deps = [
        "//tests:base",
        ":hs-lib",
    ],
)
