load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_test",
)

package(default_testonly = 1)

haskell_test(
    name = "binary-dynamic",
    srcs = ["Main.hs"],
    linkstatic = False,
    visibility = ["//visibility:public"],
    deps = ["//tests/hackage:base"],
)
