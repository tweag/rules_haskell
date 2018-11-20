package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
)

haskell_library(
    name = "library-static-only",
    srcs = ["Lib.hs"],
    visibility = ["//visibility:public"],
    deps = [
        "@hackage//:base",
        "@hackage//:bytestring",
    ],
    linkstatic = True,
)
