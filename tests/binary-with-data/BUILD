load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_test",
)

package(default_testonly = 1)

haskell_test(
    name = "bin1",
    srcs = ["bin1.hs"],
    # Regular file input:
    data = ["bin1-input"],
    visibility = ["//visibility:public"],
    deps = ["//tests/hackage:base"],
)

haskell_test(
    name = "binary-with-data",
    srcs = ["bin2.hs"],
    args = ["$(location :bin1)"],
    data = [":bin1"],
    visibility = ["//visibility:public"],
    deps = [
        "//tests/hackage:base",
        "@hackage//:process",
    ],
)
