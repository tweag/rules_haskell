load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_import",
    "haskell_library",
)

haskell_library(
    name = "library-with-two-cbits",
    srcs = ["AddThree.hsc"],
    linkstatic = False,
    visibility = ["//visibility:public"],
    deps = [
        "//tests/data:ourclibrary",
        "//tests/data:ourotherclibrary",
        "//tests/hackage:base",
    ],
)

haskell_library(
    name = "library-with-two-cbits-indirect",
    srcs = ["AddThree2.hs"],
    visibility = ["//visibility:public"],
    deps = [
        ":library-with-two-cbits",
        "//tests/hackage:base",
    ],
)

haskell_library(
    name = "library-with-two-cbits-static",
    srcs = ["AddThree.hsc"],
    visibility = ["//visibility:public"],
    deps = [
        "//tests/data:ourclibrary-static",
        "//tests/data:ourotherclibrary-static",
        "//tests/hackage:base",
    ],
)
