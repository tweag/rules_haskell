package(default_testonly = 1)

load("@io_tweag_rules_haskell//haskell:c2hs.bzl", "c2hs_library")
load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_cc_import",
)

c2hs_library(
    name = "baz",
    srcs = ["Baz.chs"],
    deps = ["@io_tweag_rules_haskell//tests/c2hs:zlib"],
    visibility = ["//visibility:public"],
)
