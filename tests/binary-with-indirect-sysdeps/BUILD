load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
    "haskell_test",
)

package(default_testonly = 1)

haskell_library(
    name = "hs-lib",
    srcs = ["HsLib.hs"],
    tags = ["requires_zlib"],
    deps = [
        "//tests/hackage:base",
        "@zlib.dev//:zlib",
    ],
)

haskell_test(
    name = "binary-with-indirect-sysdeps",
    srcs = ["Main.hs"],
    tags = ["requires_zlib"],
    deps = [
        ":hs-lib",
        "//tests/hackage:base",
    ],
)
