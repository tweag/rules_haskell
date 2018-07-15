package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
    "haskell_prebuilt_library",
    "haskell_test",
)

haskell_prebuilt_library(
  name = "bytestring",
)

haskell_prebuilt_library(
  name = "text_pkg",
  package = "text",
)

haskell_library(
    name = "Lib",
    srcs = ["Lib.hs"],
    prebuilt_dependencies = ["base"],
    deps = [":bytestring"],
)

haskell_test(
    name = "binary",
    srcs = ["Bin.hs"],
    main_file = "Bin.hs",
    prebuilt_dependencies = ["base"],
    deps = [
        ":text_pkg",
        ":Lib",
    ],
)
