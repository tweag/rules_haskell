package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
)

haskell_library(
    name = "sublib",
    srcs = ["Bar.hs"],
    prebuilt_dependencies = ["base"],
    visibility = ["//visibility:public"],
)
