load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
)

package(default_testonly = 1)

haskell_library(
    name = "sublib",
    srcs = ["Bar.hs"],
    visibility = ["//visibility:public"],
    deps = ["//tests/hackage:base"],
)
