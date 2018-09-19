package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_binary",
    "haskell_doctest",
)

haskell_binary(
    name = "binary-with-prebuilt",
    srcs = ["Main.hs"],
    visibility = ["//visibility:public"],
    deps = [
        "//tests:template-haskell",
        "//tests:void",
        "//tests:base",
    ],
)

haskell_doctest(
    name = "doctest",
    deps = [":binary-with-prebuilt"],
)
