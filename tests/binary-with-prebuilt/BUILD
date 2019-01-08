load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_test",
)

package(default_testonly = 1)

haskell_test(
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
