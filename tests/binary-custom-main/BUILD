load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_binary",
)

package(default_testonly = 1)

haskell_binary(
    name = "binary-custom-main",
    srcs = ["foo.hs"],
    visibility = ["//visibility:public"],
    deps = ["@hackage//:base"],
)
