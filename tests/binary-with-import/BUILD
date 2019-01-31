load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
    "haskell_test",
)

package(default_testonly = 1)

haskell_library(
    name = "lib",
    srcs = ["src/Lib.hs"],
    deps = [
        "//tests/hackage:base",
        "//tests/hackage:transformers",
    ],
)

haskell_test(
    name = "binary-with-import",
    srcs = ["Main.hs"],
    visibility = ["//visibility:public"],
    deps = [
        ":lib",
        "//tests/hackage:base",
    ],
)
