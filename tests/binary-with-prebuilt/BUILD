load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_test",
)

package(default_testonly = 1)

haskell_test(
    name = "binary-with-prebuilt",
    srcs = ["Main.hs"],
    tags = ["requires_hackage"],
    visibility = ["//visibility:public"],
    deps = [
        "//tests/hackage:base",
        "//tests/hackage:template-haskell",
        "@hackage//:streaming",
        "@hackage//:void",
    ],
)
