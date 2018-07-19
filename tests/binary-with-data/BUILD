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
    main_file = "bin1.hs",
    prebuilt_dependencies = ["base"],
    visibility = ["//visibility:public"],
)

haskell_binary(
    name = "binary-with-data",
    srcs = ["bin2.hs"],
    args = ["$(location :bin1)"],
    data = [":bin1"],
    main_file = "bin2.hs",
    prebuilt_dependencies = [
        "base",
        "process",
    ],
    visibility = ["//visibility:public"],
)
