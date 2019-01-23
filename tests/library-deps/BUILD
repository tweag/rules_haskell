load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_import_boot",
    "haskell_library",
    "haskell_test",
)

package(default_testonly = 1)

haskell_import_boot()

haskell_library(
    name = "library-deps",
    srcs = ["TestLib.hs"],
    visibility = ["//visibility:public"],
    deps = [
        "//tests/library-deps/sublib",
        ":base",
    ],
)

haskell_test(
    name = "bin-deps",
    size = "small",
    srcs = ["Bin.hs"],
    visibility = ["//visibility:public"],
    deps = [
        "//tests/library-deps/sublib",
        ":base",
    ],
)
