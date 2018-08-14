load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_cc_import",
    "haskell_library",
)

cc_library(
    name = "cbits",
    hdrs = [
        "include/vector.h",
        "internal/unbox-tuple-instances",
    ],
)

haskell_library(
    name = "vector",
    testonly = 1,
    srcs = glob(["Data/**/*.*hs"]),
    prebuilt_dependencies = [
        "base",
        "deepseq",
        "ghc-prim",
        "primitive",
        "semigroups",
    ],
    visibility = ["//visibility:public"],
    deps = [":cbits"],
)
