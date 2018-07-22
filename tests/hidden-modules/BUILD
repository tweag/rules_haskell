package(
    default_testonly = 1,
    default_visibility = ["//visibility:public"],
)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
)

haskell_library(
    name = "lib-a",
    srcs = glob(["lib-a/*.hs"]),
    hidden_modules = ["Foo"],
    src_strip_prefix = "lib-a",
    deps = ["//tests:base"],
)

haskell_library(
    name = "lib-b",
    srcs = glob(["lib-b/*.hs"]),
    src_strip_prefix = "lib-b",
    deps = ["//tests:base"],
)

haskell_library(
    name = "lib-c",
    srcs = glob(["lib-c/*.hs"]),
    src_strip_prefix = "lib-c",
    deps = [
        ":lib-a",
        ":lib-b",
        "//tests:base",
    ],
)
