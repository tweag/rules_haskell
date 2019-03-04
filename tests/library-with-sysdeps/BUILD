load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
    "haskell_test",
)

package(default_testonly = 1)

haskell_library(
    name = "library-with-sysdeps",
    srcs = ["Lib.hs"],
    tags = ["requires_zlib"],
    visibility = ["//visibility:public"],
    deps = [
        "@zlib",
        "//tests/hackage:base",
    ],
)

haskell_test(
    name = "bin",
    srcs = ["Main.hs"],
    tags = ["requires_zlib"],
    deps = [
        ":library-with-sysdeps",
        "//tests/hackage:base",
    ],
)
