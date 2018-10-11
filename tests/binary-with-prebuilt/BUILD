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
        "@hackage//:base",
        "@hackage//:streaming",
        "@hackage//:template-haskell",
        "@hackage//:void",
    ],
)
