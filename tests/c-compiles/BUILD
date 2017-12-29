package(default_testonly = 1)

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_binary",
)

haskell_library(
  name = "c-compiles-lib",
  srcs = ["Lib.hs"],
  c_sources = ["c-compiles.c"],
  prebuilt_dependencies = ["base"],
)

haskell_binary(
  name = "c-compiles",
  srcs = ["Main.hs",],
  prebuilt_dependencies = ["base"],
  deps = [":c-compiles-lib"],
  visibility = ["//visibility:public"],
)
