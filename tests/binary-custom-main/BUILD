package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_binary",
)

haskell_binary(
    name = "binary-custom-main",
    srcs = ["foo.hs"],
    main_file = "foo.hs",
    visibility = ["//visibility:public"],
    deps = ["//tests:base"],
)
