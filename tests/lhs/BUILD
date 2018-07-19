package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_binary",
    "haskell_library",
)

haskell_library(
    name = "lhs-lib",
    srcs = ["Lib.lhs"],
    prebuilt_dependencies = ["base"],
)

haskell_binary(
    name = "lhs-bin",
    srcs = ["Main.lhs"],
    prebuilt_dependencies = ["base"],
    visibility = ["//visibility:public"],
    deps = [":lhs-lib"],
)
