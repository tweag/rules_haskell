load("@io_tweag_rules_haskell//haskell:haskell.bzl",
     "haskell_doctest",
     "haskell_library",
     "haskell_test",
)
load("@ai_formation_hazel//:hazel.bzl", "hazel_library")

haskell_test(
    name = "vector-test",
    srcs = ["vector-test.hs"],
    deps = [
        hazel_library("base"),
        hazel_library("vector"),
    ],
)

haskell_test(
    name = "old-time-test",
    srcs = ["old-time-test.hs"],
    deps = [
        hazel_library("base"),
        hazel_library("old-time"),
    ],
)

haskell_library(
    name = "DoctestExample",
    srcs = ["DoctestExample.hs"],
    deps = [hazel_library("base")],
)

haskell_doctest(
    name = "doctest-test",
    deps = [":DoctestExample"],
)
