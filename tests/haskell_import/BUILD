package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_import",
    "haskell_library",
    "haskell_test",
)

haskell_library(
    name = "Lib",
    srcs = ["Lib.hs"],
    deps = [
        "@hackage//:bytestring",
        "@hackage//:base",
    ],
)

haskell_test(
    name = "binary",
    srcs = ["Bin.hs"],
    deps = [
        ":Lib",
        "@hackage//:base",
        "@hackage//:text",
    ],
)
