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
    prebuilt_dependencies = [
        "base",
    ],
)

haskell_library(
    name = "two",
    srcs = ["Two.hs"],
    prebuilt_dependencies = [
        "base",
    ],
    deps = [":one"],
)

haskell_binary(
    name = "two-libs",
    srcs = ["Main.hs"],
    prebuilt_dependencies = ["base"],
    deps = [":two"],
)
