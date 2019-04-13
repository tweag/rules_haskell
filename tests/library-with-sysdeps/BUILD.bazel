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
        "//tests/hackage:base",
        "@zlib",
    ],
)

haskell_test(
    name = "bin",
    srcs = ["Main.hs"],
    expected_covered_expressions_percentage = 100,
    tags = [
        "coverage-compatible",
        "requires_zlib",
    ],
    deps = [
        ":library-with-sysdeps",
        "//tests/hackage:base",
    ],
)
