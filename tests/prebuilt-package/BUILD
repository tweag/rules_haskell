package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
    "haskell_prebuilt_package",
    "haskell_test",
)

haskell_prebuilt_package(
  name = "bytestring",
  package = "bytestring",
)

haskell_prebuilt_package(
  name = "text",
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
        ":text",
        ":Lib",
    ],
)
