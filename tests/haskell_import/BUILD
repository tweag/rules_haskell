load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_import_boot",
    "haskell_library",
    "haskell_test",
)

package(default_testonly = 1)

haskell_import_boot()

haskell_library(
    name = "Lib",
    srcs = ["Lib.hs"],
    deps = [
        ":base",
        "@hackage//:bytestring",
    ],
)

haskell_test(
    name = "binary",
    srcs = ["Bin.hs"],
    deps = [
        ":Lib",
        ":base",
        "@hackage//:text",
    ],
)
