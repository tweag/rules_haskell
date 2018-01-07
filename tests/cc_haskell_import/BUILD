package(default_testonly = 1)

load("@io_tweag_rules_haskell//haskell:haskell.bzl",
   "haskell_library",
   "cc_haskell_import",
)

haskell_library(
  name = "hs-lib-a",
  srcs = ["LibA.hs"],
  prebuilt_dependencies = ["base"],
)

haskell_library(
  name = "hs-lib-b",
  srcs = ["LibB.hs"],
  prebuilt_dependencies = ["base"],
  deps = [":hs-lib-a"],
)

cc_haskell_import(
  name = "hs-lib-b-so",
  dep = ":hs-lib-b",
  visibility = ["//tests:__subpackages__"],
)

cc_binary(
  name = "cc-bin",
  srcs = ["main.c", ":hs-lib-b-so"],
  deps = ["@ghc//:threaded-rts"],
  visibility = ["//tests:__subpackages__"],
)
