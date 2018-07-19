load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_binary",
    "haskell_library",
    "haskell_test",
)

haskell_library(
    name = "runfiles",
    srcs = ["Bazel/Runfiles.hs"],
    prebuilt_dependencies = [
        "base",
        "directory",
        "filepath",
    ],
    visibility = ["//visibility:public"],
)

haskell_binary(
    name = "bin",
    testonly = 1,
    srcs = ["Bin.hs"],
    data = ["bin-data.txt"],
    main_file = "Bin.hs",
    prebuilt_dependencies = [
        "base",
        "filepath",
    ],
    deps = [":runfiles"],
)

haskell_test(
    name = "test",
    srcs = ["Test.hs"],
    data = [
        "test-data.txt",
        ":bin",
    ],
    main_file = "Test.hs",
    prebuilt_dependencies = [
        "base",
        "filepath",
        "process",
    ],
    deps = [":runfiles"],
)
