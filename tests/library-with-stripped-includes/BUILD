load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
)

package(default_testonly = 1)

cc_library(
    name = "cbits-indirect",
    includes = [
        "includes-a",
    ],
    hdrs = [
        "includes-a/a.h",
    ],
)

cc_library(
    name = "cbits-direct",
    includes = [
        "includes-b"
    ],
    hdrs = [
        "includes-b/b.h",
    ],
    deps = [
        ":cbits-indirect",
    ],
)

haskell_library(
    name = "library-with-stripped-includes",
    srcs = ["Lib.hs"],
    deps = [
        ":cbits-direct",
        "@hackage//:base",
    ],
)

cc_library(
    name = "intermediate-clib",
    deps = [":clib"],
    srcs = ["intermediate-clib.c"],
)

cc_library(
    name = "final-clib",
    deps = [":intermediate-clib"],
    srcs = ["final-clib.c"],
)
