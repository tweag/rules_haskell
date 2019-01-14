load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_import",
    "haskell_library",
)

haskell_library(
    name = "library-with-cbits",
    srcs = ["AddOne.hsc"],
    visibility = ["//visibility:public"],
    deps = [
        "//tests/data:ourclibrary",
        "@hackage//:base",
    ],
)

haskell_library(
    name = "library-with-cbits-static",
    srcs = ["AddOne.hsc"],
    visibility = ["//visibility:public"],
    deps = [
        ":ourclibrary-static",
        "@hackage//:base",
    ],
)

cc_library(
    name = "ourclibrary-static",
    linkstatic = True,
    deps = [
        "//tests/data:ourclibrary",
    ],
)
