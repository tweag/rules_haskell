load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_import_boot",
    "haskell_library",
    "haskell_test",
)

package(default_testonly = 1)

haskell_import_boot()

haskell_library(
    name = "hs-lib",
    srcs = ["Lib.hs"],
    deps = [
        "//tests/data:ourclibrary",
        ":base",
    ],
)

haskell_test(
    name = "c-compiles",
    srcs = ["Main.hs"],
    visibility = ["//visibility:public"],
    deps = [
        ":hs-lib",
        ":base",
    ],
)
