package(default_testonly = 1)

load("@io_tweag_rules_haskell//haskell:haskell.bzl",
     "haskell_binary",
)

haskell_binary(
  name = "paths-module",
  srcs = ["Main.hs"],
  prebuilt_dependencies = ["base"],
  visibility = ["//visibility:public"]
)
