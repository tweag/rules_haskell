load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_import_boot",
    "haskell_test",
)

package(default_testonly = 1)

haskell_import_boot()

haskell_test(
    name = "binary-with-prebuilt",
    srcs = ["Main.hs"],
    visibility = ["//visibility:public"],
    deps = [
        ":base",
        "@hackage//:streaming",
        "@hackage//:template-haskell",
        "@hackage//:void",
    ],
)
