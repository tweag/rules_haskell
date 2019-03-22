load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
    "haskell_test",
)

package(default_testonly = 1)

haskell_library(
    name = "Lib",
    srcs = ["Lib.hs"],
    deps = [
        "//tests/hackage:base",
        "//tests/hackage:bytestring",
    ],
)

haskell_test(
    name = "binary",
    srcs = ["Bin.hs"],
    deps = [
        ":Lib",
        "//tests/hackage:base",
        "//tests/hackage:text",
    ],
)
