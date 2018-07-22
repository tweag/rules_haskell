package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_binary",
    "haskell_library",
)

haskell_library(
    name = "lhs-lib",
    srcs = ["Lib.lhs"],
    deps = ["//tests:base"],
)

haskell_binary(
    name = "lhs-bin",
    srcs = ["Main.lhs"],
    visibility = ["//visibility:public"],
    deps = [
        ":lhs-lib",
        "//tests:base",
    ],
)
