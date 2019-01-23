load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_import_boot",
    "haskell_library",
    "haskell_test",
)

package(default_testonly = 1)

haskell_import_boot()

haskell_library(
    name = "lhs-lib",
    srcs = ["Lib.lhs"],
    deps = [":base"],
)

haskell_test(
    name = "lhs-bin",
    srcs = ["Main.lhs"],
    visibility = ["//visibility:public"],
    deps = [
        ":lhs-lib",
        ":base",
    ],
)
