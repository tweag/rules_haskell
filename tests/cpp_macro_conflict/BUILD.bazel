load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
    "haskell_test",
)

package(default_testonly = 1)

# empty library with package name "bytestring"
haskell_library(
    name = "bytestring",
    srcs = ["src/BS.hs"],
    deps = ["//tests/hackage:base"],
)

# This depends on two packages "bytestring"
# There should be no CPP macro conflict
haskell_test(
    name = "macro_conflict",
    srcs = ["Main.hs"],
    compiler_flags = [
        "-XCPP",
        "-Werror",
    ] + select({
        # clang on darwin fails because of unused command line argument, it fails because of -Werror
        "@bazel_tools//src/conditions:darwin": [
            "-optP-Wno-unused-command-line-argument",
        ],
        "//conditions:default": [],
    }),
    deps = [
        ":bytestring",
        "//tests/hackage:base",
        "//tests/hackage:bytestring",
    ],
)
