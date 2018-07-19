package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_binary",
)

haskell_binary(
    name = "binary-with-prebuilt",
    srcs = ["Main.hs"],
    prebuilt_dependencies = [
        "base",
        "template-haskell",
    ],
    visibility = ["//visibility:public"],
)
