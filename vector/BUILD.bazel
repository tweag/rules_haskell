load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_cc_import",
    "haskell_import",
    "haskell_library",
)

haskell_import(name = "base")
haskell_import(name = "deepseq")
haskell_import(name = "ghc-prim")
haskell_import(name = "primitive")
haskell_import(name = "semigroups")

haskell_library(
    name = "vector",
    testonly = 1,
    srcs = glob(["Data/**/*.*hs"]),
    extra_srcs = ["include/vector.h", "internal/unbox-tuple-instances"],
    compiler_flags = ["-Ivector/include", "-Ivector/internal"],
    deps = [
        ":base",
        ":deepseq",
        ":ghc-prim",
        ":primitive",
        ":semigroups",
    ],
    visibility = ["//visibility:public"],
)
