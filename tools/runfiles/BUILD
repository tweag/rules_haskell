load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
    "haskell_test",
)

haskell_library(
    name = "runfiles",
    srcs = ["src/Bazel/Runfiles.hs"],
    visibility = ["//visibility:public"],
    deps = [
        "@hackage//:base",
        "@hackage//:directory",
        "@hackage//:filepath",
    ],
)

haskell_test(
    name = "bin",
    testonly = 1,
    srcs = ["bin/Bin.hs"],
    data = ["bin-data.txt"],
    deps = [
        ":runfiles",
        "@hackage//:base",
        "@hackage//:filepath",
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
        "@hackage//:base",
        "@hackage//:filepath",
        "@hackage//:process",
    ],
)
