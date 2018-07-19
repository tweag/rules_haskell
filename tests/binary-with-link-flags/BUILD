package(
    default_testonly = 1,
    default_visibility = ["//visibility:public"],
)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_binary",
)

haskell_binary(
    name = "binary-with-link-flags",
    srcs = ["Main.hs"],
    compiler_flags = ["-threaded"],
    prebuilt_dependencies = ["base"],
    visibility = ["//visibility:public"],
)
