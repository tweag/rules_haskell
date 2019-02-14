load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_cc_import",
    "haskell_test",
)

package(default_testonly = 1)

haskell_cc_import(
    name = "zlib",
    shared_library = "@zlib//:lib",
    tags = ["requires_zlib"],
)

haskell_test(
    name = "binary-with-sysdeps",
    srcs = ["Main.hs"],
    tags = ["requires_zlib"],
    visibility = ["//visibility:public"],
    deps = [
        ":zlib",
        "//tests/hackage:base",
        "@gmp",
    ],
)
