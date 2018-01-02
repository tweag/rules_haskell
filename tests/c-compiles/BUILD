package(default_testonly = 1)

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_binary",
)

cc_library(
  name = "c-lib",
  srcs = ["c-compiles.c"],
)

haskell_library(
  name = "hs-lib",
  srcs = ["Lib.hs"],
  deps = [":c-lib"],
  prebuilt_dependencies = ["base"],
)

haskell_binary(
  name = "c-compiles",
  srcs = ["Main.hs",],
  prebuilt_dependencies = ["base"],
  deps = [":hs-lib"],
  visibility = ["//visibility:public"],
)
