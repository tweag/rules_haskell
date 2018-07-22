package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_binary",
    "haskell_library",
)

haskell_library(
    name = "lib",
    srcs = glob(["src/*.hs"]),
    src_strip_prefix = "src",
)

haskell_binary(
    name = "binary-with-lib",
    srcs = ["Main.hs"],
    visibility = ["//visibility:public"],
    deps = [
        ":lib",
        "//tests:base",
    ],
)
