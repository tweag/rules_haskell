package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
)

haskell_library(
    name = "sublib",
    srcs = ["TestSubLib.hs"],
    visibility = ["//visibility:public"],
    deps = [
        ":sublib-c",
        "//tests:base",
    ],
)

cc_library(
    name = "sublib-c",
    srcs = ["sublib-c.c"],
)
