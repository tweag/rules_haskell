load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_cc_import",
    "haskell_library",
    "haskell_import_boot",
    "haskell_test",
)

package(default_testonly = 1)

haskell_import_boot()

haskell_cc_import(
    name = "zlib",
    shared_library = "@zlib//:lib",
)

haskell_library(
    name = "library-with-sysdeps",
    srcs = ["Lib.hs"],
    visibility = ["//visibility:public"],
    deps = [
        ":zlib",
        ":base",
    ],
)

haskell_test(
    name = "bin",
    srcs = ["Main.hs"],
    deps = [
        ":library-with-sysdeps",
        ":base",
    ],
)
