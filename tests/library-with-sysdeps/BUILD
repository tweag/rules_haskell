package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_binary",
    "haskell_cc_import",
    "haskell_library",
)

haskell_cc_import(
    name = "zlib",
    shared_library = "@zlib//:lib",
)

haskell_library(
    name = "library-with-sysdeps",
    srcs = ["Lib.hs"],
    visibility = ["//visibility:public"],
    deps = [
        ":zlib",
        "//tests:base",
    ],
)

haskell_binary(
    name = "bin",
    srcs = ["Main.hs"],
    deps = [
        ":library-with-sysdeps",
        "//tests:base",
    ],
)
