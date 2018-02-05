package(default_testonly = 1)

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_binary",
)

haskell_library(
  name = "hs-lib",
  srcs = ["Lib.hs"],
  prebuilt_dependencies = ["base"],
  c_srcs = ["c-compiles.c"],
)

haskell_binary(
  name = "c-compiles",
  srcs = ["Main.hs",],
  prebuilt_dependencies = ["base"],
  deps = [":hs-lib"],
  visibility = ["//visibility:public"],
)
