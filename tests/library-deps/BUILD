package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
    "haskell_test",
)

haskell_library(
    name = "library-deps",
    srcs = ["TestLib.hs"],
    prebuilt_dependencies = ["base"],
    visibility = ["//visibility:public"],
    deps = ["//tests/library-deps/sublib"],
)

haskell_test(
    name = "bin-deps",
    size = "small",
    srcs = ["Bin.hs"],
    main_file = "Bin.hs",
    prebuilt_dependencies = ["base"],
    visibility = ["//visibility:public"],
    deps = ["//tests/library-deps/sublib"],
)
