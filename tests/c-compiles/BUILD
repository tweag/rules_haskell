load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_binary",
    "haskell_library",
)

package(default_testonly = 1)

haskell_library(
    name = "hs-lib",
    srcs = ["Lib.hs"],
    deps = [
        "//tests/data:ourclibrary",
        "@hackage//:base",
    ],
)

haskell_binary(
    name = "c-compiles",
    srcs = ["Main.hs"],
    visibility = ["//visibility:public"],
    deps = [
        ":hs-lib",
        "@hackage//:base",
    ],
)
