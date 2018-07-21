package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
)

haskell_library(
    name = "sublib",
    srcs = ["Foo.hs"],
    visibility = ["//visibility:public"],
    deps = ["//tests:base"],
)

haskell_library(
    name = "lib",
    srcs = ["Baz.hs"],
    visibility = ["//visibility:public"],
    deps = [
        ":sublib",
        "//tests:base",
        "//tests/package-id-clash/sublib",
    ],
)
