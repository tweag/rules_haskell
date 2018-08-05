package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
    "haskell_test",
)

haskell_library(
    name = "sublib",
    srcs = ["TestSubLib.hs"],
    exports = {"//tests:base": "Data.List as SubLib.List"},
    deps = ["//tests:base"],
)

haskell_library(
    name = "lib",
    srcs = ["TestLib.hs"],
    deps = ["//tests:base", ":sublib"],
    exports = {
        ":sublib": "TestSubLib",
        "//tests:base": "Data.List as Lib.List",
    },
)

haskell_test(
    name = "library-exports",
    size = "small",
    srcs = ["Bin.hs"],
    visibility = ["//visibility:public"],
    deps = [":lib", "//tests:base"],
)
