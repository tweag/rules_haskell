package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_binary",
)

haskell_binary(
    name = "bin1",
    srcs = ["bin1.hs"],
    # Regular file input:
    data = ["bin1-input"],
    visibility = ["//visibility:public"],
    deps = ["//tests:base"],
)

haskell_binary(
    name = "binary-with-data",
    srcs = ["bin2.hs"],
    args = ["$(location :bin1)"],
    data = [":bin1"],
    visibility = ["//visibility:public"],
    deps = [
        "//tests:base",
        "//tests:process",
    ],
)
