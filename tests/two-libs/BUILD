package(
    default_testonly = 1,
    default_visibility = ["//visibility:public"],
)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_binary",
    "haskell_library",
)

haskell_library(
    name = "one",
    srcs = ["One.hs"],
    deps = [
        "//tests:base",
    ],
)

haskell_library(
    name = "two",
    srcs = ["Two.hs"],
    deps = [
        ":one",
        "//tests:base",
    ],
)

haskell_binary(
    name = "two-libs",
    srcs = ["Main.hs"],
    deps = [
        ":two",
        "//tests:base",
    ],
)
