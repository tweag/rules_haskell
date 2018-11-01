load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_binary",
    "haskell_library",
    "haskell_test",
)

haskell_library(
    name = "runfiles",
    srcs = ["src/Bazel/Runfiles.hs"],
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
    srcs = ["bin/Bin.hs"],
    data = ["bin-data.txt"],
    deps = [
        ":runfiles",
        "//tests:base",
        "//tests:filepath",
    ],
)

haskell_test(
    name = "test",
    srcs = ["test/Test.hs"],
    data = [
        "test-data.txt",
        ":bin",
    ],
    deps = [
        ":runfiles",
        "//tests:base",
        "//tests:filepath",
        "//tests:process",
    ],
)
