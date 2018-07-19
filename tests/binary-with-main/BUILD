package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_binary",
)

haskell_binary(
    name = "binary-with-main",
    srcs = ["MainIsHere.hs"],
    main_function = "MainIsHere.this",
    prebuilt_dependencies = ["base"],
    visibility = ["//visibility:public"],
)
