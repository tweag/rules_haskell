load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_binary",
  "haskell_library",
)

haskell_library(
  name = "lib",
  srcs = glob(["src/*.hs"]),
  sourceDir = "src",
)

haskell_binary(
  name = "binary-with-lib",
  srcs = ["Main.hs"],
  deps = ["lib"],
  visibility = ["//visibility:public"],
)
