package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
    "haskell_test",
)

haskell_library(
    name = "sublib",
    srcs = ["TestSubLib.hs"],
    exports = {"//tests:containers": "Data.Map as SubLib.Map"},
    deps = [
        "//tests:base",
        "//tests:containers",
    ],
)

haskell_library(
    name = "lib",
    srcs = ["TestLib.hs"],
    exports = {
        ":sublib": "TestSubLib",
        "//tests:containers": "Data.Map as Lib.Map",
    },
    deps = [
        ":sublib",
        "//tests:base",
    ],
)

haskell_test(
    name = "library-exports",
    size = "small",
    srcs = ["Bin.hs"],
    visibility = ["//visibility:public"],
    deps = [
        ":lib",
        "//tests:base",
    ],
)
