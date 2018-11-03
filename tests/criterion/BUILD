package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
    "haskell_criterion",
)

haskell_library(
    name = "mylib",
    srcs = ["Lib.hs"],
    deps = ["//tests:base"],
)

haskell_criterion(
    name = "criterion",
    srcs = ["Bench.hs"],
    deps = [
        ":mylib",
        "//tests:base",
        "//tests:criterion",
    ],
)
