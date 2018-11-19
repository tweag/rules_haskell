package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_binary",
)

haskell_binary(
    name = "binary-mostly-static",
    srcs = ["Main.hs"],
    visibility = ["//visibility:public"],
    deps = ["@hackage//:base"],
    linkstatic = False,
)
