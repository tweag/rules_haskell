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
    prebuilt_dependencies = ["base"],
    src_strip_prefix = "lib-a",
)

haskell_library(
    name = "lib-b",
    srcs = glob(["lib-b/*.hs"]),
    prebuilt_dependencies = ["base"],
    src_strip_prefix = "lib-b",
)

haskell_library(
    name = "lib-c",
    srcs = glob(["lib-c/*.hs"]),
    prebuilt_dependencies = ["base"],
    src_strip_prefix = "lib-c",
    deps = [
        ":lib-a",
        ":lib-b",
    ],
)
