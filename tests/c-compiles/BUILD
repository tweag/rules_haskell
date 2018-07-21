package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_binary",
    "haskell_library",
)

cc_library(
    name = "c-lib",
    srcs = ["c-compiles.c"],
    visibility = ["//visibility:public"],
)

haskell_library(
    name = "hs-lib",
    srcs = ["Lib.hs"],
    deps = [
        ":c-lib",
        "//tests:base",
    ],
)

haskell_binary(
    name = "c-compiles",
    srcs = ["Main.hs"],
    visibility = ["//visibility:public"],
    deps = [
        ":hs-lib",
        "//tests:base",
    ],
)
