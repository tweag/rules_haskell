load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
    "haskell_test",
    "haskell_repl",
)

package(default_testonly = 1)

haskell_library(
    name = "library-deps",
    srcs = ["TestLib.hs"],
    visibility = ["//visibility:public"],
    deps = [
        "//tests/hackage:base",
        "//tests/library-deps/sublib",
    ],
)

haskell_test(
    name = "bin-deps",
    size = "small",
    srcs = ["Bin.hs"],
    visibility = ["//visibility:public"],
    deps = [
        "//tests/hackage:base",
        "//tests/library-deps/sublib",
    ],
)

haskell_repl(
    name = "library-repl",
    deps = [
        ":library-deps",
        ":bin-deps",
    ],
)
