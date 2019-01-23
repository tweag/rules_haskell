load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_import_boot",
    "haskell_library",
)

package(default_testonly = 1)

haskell_import_boot()

haskell_library(
    name = "sublib",
    srcs = ["TestSubLib.hs"],
    visibility = ["//visibility:public"],
    deps = [
        ":sublib-c",
        ":base",
    ],
)

cc_library(
    name = "sublib-c",
    srcs = ["sublib-c.c"],
)
