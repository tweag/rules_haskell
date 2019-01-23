load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_import",
    "haskell_test",
)

package(default_testonly = 1)

haskell_import(
    name = "base_pkg",
    package = "base",
)

cc_test(
    name = "binary-simple",
    srcs = ["main.cc"],
    visibility = ["//visibility:public"],
)
