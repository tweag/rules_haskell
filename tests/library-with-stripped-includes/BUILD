load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
)

package(default_testonly = 1)

cc_library(
    name = "clib",
    includes = ["includes-a", "includes-b"],
    hdrs = [
        "includes-a/a.h",
        "includes-b/b.h",
    ],
)

haskell_library(
    name = "intermediate",
    srcs = ["Intermediate.hs"],
    deps = [
        ":clib",
        "@hackage//:base",
    ],
)

haskell_library(
    name = "library-with-stripped-includes",
    srcs = ["Lib.hs"],
    deps = [
        ":intermediate",
        "@hackage//:base",
    ],
)
