load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_binary",
    "haskell_library",
    "haskell_test",
)

haskell_library(
    name = "runfiles",
    srcs = ["Bazel/Runfiles.hs"],
    visibility = ["//visibility:public"],
    deps = [
        "//tests:base",
        "//tests:directory",
        "//tests:filepath",
    ],
)

haskell_binary(
    name = "bin",
    testonly = 1,
    srcs = ["Bin.hs"],
    data = ["bin-data.txt"],
    main_file = "Bin.hs",
    deps = [
        ":runfiles",
        "//tests:base",
        "//tests:filepath",
    ],
)

haskell_test(
    name = "test",
    srcs = ["Test.hs"],
    data = [
        "test-data.txt",
        ":bin",
    ],
    main_file = "Test.hs",
    deps = [
        ":runfiles",
        "//tests:base",
        "//tests:filepath",
        "//tests:process",
    ],
)
