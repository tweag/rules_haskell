package(
    default_testonly = 1,
    default_visibility = ["//visibility:public"],
)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_cc_import",
    "haskell_doc",
    "haskell_library",
)

haskell_cc_import(
    name = "zlib",
    shared_library = "@zlib//:lib",
)

haskell_library(
    name = "haddock-lib-deep",
    srcs = ["Deep.hsc"],
    prebuilt_dependencies = ["base"],
)

haskell_library(
    name = "haddock-lib-a",
    srcs = [
        "LibA.hs",
        "LibA/A.hs",
        "header.h",
    ],
    compiler_flags = ["-I."],
    prebuilt_dependencies = [
        "base",
        "template-haskell",
    ],
    deps = [":haddock-lib-deep"],
)

haskell_library(
    name = "haddock-lib-b",
    srcs = ["LibB.hs"],
    prebuilt_dependencies = ["base"],
    deps = [
        ":haddock-lib-a",
        ":zlib",
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
