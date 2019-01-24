load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_test",
)

package(default_testonly = 1)

haskell_test(
    name = "binary-simple",
    srcs = ["Main.hs"],
    visibility = ["//visibility:public"],
    deps = [
        "//tests/data:ourclibrary",
        "//tests/hackage:base",
    ],
)

cc_binary(
    name = "ccbin",
    srcs = ["mybin.c"],
    visibility = ["//visibility:public"],
)
