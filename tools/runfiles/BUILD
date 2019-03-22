load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
    "haskell_test",
)

haskell_library(
    name = "runfiles",
    srcs = ["src/Bazel/Runfiles.hs"],
    src_strip_prefix = "src",
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
    src_strip_prefix = "bin",
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
    src_strip_prefix = "test",
    deps = [
        ":runfiles",
        "@hackage//:base",
        "@hackage//:filepath",
        "@hackage//:process",
    ],
)
