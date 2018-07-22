package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
    "haskell_test",
)

cc_library(
    name = "cbits",
    srcs = ["cbits.c"],
)

haskell_library(
    name = "mylib",
    srcs = ["Lib.hs"],
    deps = ["//tests:base"],
)

haskell_test(
    name = "haskell_test",
    # Use some parameters that only test rules have.
    size = "small",
    timeout = "short",
    srcs = ["Test.hs"],
    flaky = False,
    main_function = "Test.test",
    visibility = ["//visibility:public"],
    deps = [
        ":cbits",
        ":mylib",
        "//tests:base",
    ],
)
