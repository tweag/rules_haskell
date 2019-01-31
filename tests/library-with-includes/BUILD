load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_binary",
    "haskell_library",
)

package(default_testonly = 1)

genrule(
    name = "gen-a",
    outs = ["a.h"],
    cmd = "echo '#define A 42' >> $@",
)

cc_library(
    name = "clib",
    hdrs = [
        "b.h",
        ":gen-a",
    ],
)

haskell_library(
    name = "library-with-includes",
    srcs = ["src/Lib.hs"],
    deps = [
        ":clib",
        "//tests/hackage:base",
    ],
)

haskell_binary(
    name = "bin",
    srcs = ["Main.hs"],
    deps = [
        ":library-with-includes",
        "//tests/hackage:base",
    ],
    visibility = ["//visibility:public"],
)
