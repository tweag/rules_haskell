load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_cc_import",
    "haskell_library",
    "haskell_test",
)

package(default_testonly = 1)

haskell_cc_import(
    name = "zlib",
    shared_library = "@zlib//:lib",
    tags = ["dont_test_when_bindist"],
)

haskell_library(
    name = "library-with-sysdeps",
    srcs = ["Lib.hs"],
    tags = ["dont_test_when_bindist"],
    visibility = ["//visibility:public"],
    deps = [
        ":zlib",
        "//tests/hackage:base",
    ],
)

haskell_test(
    name = "bin",
    srcs = ["Main.hs"],
    tags = ["dont_test_when_bindist"],
    deps = [
        ":library-with-sysdeps",
        "//tests/hackage:base",
    ],
)
