load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
    "haskell_test",
)

package(
    default_testonly = 1,
    default_visibility = ["//visibility:public"],
)

haskell_library(
    name = "one",
    srcs = ["One.hs"],
    deps = [
        "//tests/hackage:base",
    ],
)

haskell_library(
    name = "two",
    srcs = ["Two.hs"],
    deps = [
        ":one",
        "//tests/hackage:base",
    ],
)

haskell_test(
    name = "two-libs",
    srcs = ["Main.hs"],
    deps = [
        ":two",
        "//tests/hackage:base",
    ],
)
