package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "cc_haskell_import",
    "haskell_binary",
    "haskell_library",
)

haskell_library(
    name = "hs-lib-a",
    srcs = ["LibA.hs"],
    deps = ["//tests:base"],
)

haskell_library(
    name = "hs-lib-b",
    srcs = ["LibB.hs"],
    deps = [
        ":hs-lib-a",
        "//tests:base",
    ],
)

cc_haskell_import(
    name = "hs-lib-b-so",
    dep = ":hs-lib-b",
    visibility = ["//tests:__subpackages__"],
)

cc_binary(
    name = "cc-bin",
    srcs = [
        "main.c",
        ":hs-lib-b-so",
    ],
    visibility = ["//tests:__subpackages__"],
    deps = ["@ghc//:threaded-rts"],
)
