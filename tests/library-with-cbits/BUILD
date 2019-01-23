load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_import_boot",
    "haskell_library",
)

haskell_import_boot()

haskell_library(
    name = "library-with-cbits",
    srcs = ["AddOne.hsc"],
    visibility = ["//visibility:public"],
    deps = [
        ":ourclibrary-indirect",
        ":base",
    ],
)

haskell_library(
    name = "library-with-cbits-indirect",
    srcs = ["AddOne2.hs"],
    visibility = ["//visibility:public"],
    deps = [
        ":library-with-cbits",
        ":base",
    ],
)

haskell_library(
    name = "library-with-cbits-static",
    srcs = ["AddOne.hsc"],
    visibility = ["//visibility:public"],
    deps = [
        ":ourclibrary-static",
        ":base",
    ],
)

cc_library(
    name = "ourclibrary-indirect",
    deps = [
        "//tests/data:ourclibrary",
    ],
)

cc_library(
    name = "ourclibrary-static",
    linkstatic = True,
    deps = [
        "//tests/data:ourclibrary",
    ],
)
