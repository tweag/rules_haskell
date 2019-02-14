load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_cc_import",
    "haskell_doc",
    "haskell_import",
    "haskell_library",
)

package(
    default_testonly = 1,
    default_visibility = ["//visibility:public"],
)

haskell_cc_import(
    name = "zlib",
    shared_library = "@zlib//:lib",
    tags = ["requires_zlib"],
)

haskell_library(
    name = "haddock-lib-deep",
    srcs = ["Deep.hsc"],
    deps = ["//tests/hackage:base", "@gmp"],
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
        "//tests/hackage:base",
        "//tests/hackage:template-haskell",
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
    tags = ["requires_hackage"],
    deps = [
        ":haddock-lib-a",
        ":zlib",
        "//tests/hackage:base",
        "//tests/hackage:template-haskell",
        "@hackage//:libc",
    ],
)

haskell_doc(
    name = "haddock",
    index_transitive_deps = False,
    tags = ["requires_hackage"],
    deps = [":haddock-lib-b"],
)

haskell_doc(
    name = "haddock-transitive",
    index_transitive_deps = True,
    tags = ["requires_hackage"],
    deps = [":haddock-lib-b"],
)
