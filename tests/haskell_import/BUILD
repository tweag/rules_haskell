package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_import",
    "haskell_library",
    "haskell_test",
)

haskell_import(
    name = "bytestring",
)

haskell_import(
    name = "text_pkg",
    package = "text",
)

haskell_library(
    name = "Lib",
    srcs = ["Lib.hs"],
    deps = [
        ":bytestring",
        "//tests:base",
    ],
)

haskell_test(
    name = "binary",
    srcs = ["Bin.hs"],
    main_file = "Bin.hs",
    deps = [
        ":Lib",
        ":text_pkg",
        "//tests:base",
    ],
)
