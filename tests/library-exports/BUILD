load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
    "haskell_test",
)

package(default_testonly = 1)

haskell_library(
    name = "sublib",
    srcs = ["TestSubLib.hs"],
    exports = {"@hackage//:containers": "Data.Map as SubLib.Map"},
    deps = [
        "@hackage//:base",
        "@hackage//:containers",
    ],
)

haskell_library(
    name = "lib",
    srcs = ["TestLib.hs"],
    exports = {
        ":sublib": "TestSubLib",
        "@hackage//:containers": "Data.Map as Lib.Map",
    },
    deps = [
        ":sublib",
        "@hackage//:base",
    ],
)

haskell_test(
    name = "library-exports",
    size = "small",
    srcs = ["Bin.hs"],
    visibility = ["//visibility:public"],
    deps = [
        ":lib",
        "@hackage//:base",
    ],
)
