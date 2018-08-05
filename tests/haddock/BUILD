package(
    default_testonly = 1,
    default_visibility = ["//visibility:public"],
)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_cc_import",
    "haskell_doc",
    "haskell_import",
    "haskell_library",
)

haskell_cc_import(
    name = "zlib",
    shared_library = "@zlib//:lib",
)

haskell_library(
    name = "haddock-lib-deep",
    srcs = ["Deep.hsc"],
    deps = ["//tests:base"],
)

haskell_library(
    name = "haddock-lib-a",
    srcs = [
        "LibA.hs",
        "LibA/A.hs",
        "header.h",
    ],
    compiler_flags = ["-I."],
    deps = [
        ":haddock-lib-deep",
        "//tests:base",
        "//tests:template-haskell",
    ],
)

haskell_import(
    name = "haddock-lib-c",
    package = "libc",
)

haskell_library(
    name = "haddock-lib-b",
    srcs = [
        "LibB.hs",
        "TH.hs",
    ],
    extra_srcs = [
        "unicode.txt",
    ],
    deps = [
        ":haddock-lib-a",
        ":haddock-lib-c",
        ":zlib",
        "//tests:base",
        "//tests:template-haskell",
    ],
)

haskell_doc(
    name = "haddock",
    index_transitive_deps = False,
    deps = [":haddock-lib-b"],
)

haskell_doc(
    name = "haddock-transitive",
    index_transitive_deps = True,
    deps = [":haddock-lib-b"],
)
