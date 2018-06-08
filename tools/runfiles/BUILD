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
    srcs = ["Bin.hs"],
    main_file = "Bin.hs",
    deps = [":runfiles"],
    data = ["bin-data.txt"],
    prebuilt_dependencies = [
        "base",
        "filepath",
    ],
    testonly = 1,
)
haskell_test(
    name = "test",
    srcs = ["Test.hs"],
    main_file = "Test.hs",
    deps = [":runfiles"],
    data = ["test-data.txt", ":bin"],
    prebuilt_dependencies = [
        "base",
        "filepath",
        "process",
    ],
)
