load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_cc_import",
)

cc_library(
  name = "cbits",
  hdrs = ["include/vector.h", "internal/unbox-tuple-instances"],
)

haskell_library(
  name = "vector",
  srcs = glob(["Data/**/*.*hs"]),
  visibility = ["//visibility:public"],
  prebuilt_dependencies = [
    "base",
    "deepseq",
    "ghc-prim",
    "primitive",
    "semigroups"
  ],
  deps = [":cbits"],
  testonly = 1,
)
