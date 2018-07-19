package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_binary",
    "haskell_library",
)

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
    srcs = ["Lib.hs"],
    prebuilt_dependencies = ["base"],
    deps = [":clib"],
)
