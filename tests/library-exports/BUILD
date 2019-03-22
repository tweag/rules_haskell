load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
    "haskell_test",
)

package(default_testonly = 1)

haskell_library(
    name = "sublib",
    srcs = ["TestSubLib.hs"],
    exports = {"//tests/hackage:containers": "Data.Map as SubLib.Map"},
    deps = [
        "//tests/hackage:base",
        "//tests/hackage:containers",
    ],
)

haskell_library(
    name = "lib",
    srcs = ["TestLib.hs"],
    exports = {
        ":sublib": "TestSubLib",
        "//tests/hackage:containers": "Data.Map as Lib.Map",
    },
    deps = [
        ":sublib",
        "//tests/hackage:base",
    ],
)

haskell_test(
    name = "library-exports",
    size = "small",
    srcs = ["Bin.hs"],
    visibility = ["//visibility:public"],
    deps = [
        ":lib",
        "//tests/hackage:base",
    ],
)
