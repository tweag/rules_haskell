package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
)

haskell_library(
    name = "sublib",
    srcs = ["Foo.hs"],
    prebuilt_dependencies = ["base"],
    visibility = ["//visibility:public"],
)

haskell_library(
    name = "lib",
    srcs = ["Baz.hs"],
    prebuilt_dependencies = ["base"],
    visibility = ["//visibility:public"],
    deps = [
        ":sublib",
        "//tests/package-id-clash/sublib",
    ],
)
