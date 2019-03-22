load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
    "haskell_test",
)

package(default_testonly = 1)

haskell_library(
    name = "lhs-lib",
    srcs = ["Lib.lhs"],
    deps = ["//tests/hackage:base"],
)

haskell_test(
    name = "lhs-bin",
    srcs = ["Main.lhs"],
    visibility = ["//visibility:public"],
    deps = [
        ":lhs-lib",
        "//tests/hackage:base",
    ],
)
