load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "cc_haskell_import",
)

haskell_library(
  name = "add-one-hs",
  srcs = ["One.hs"],
  prebuilt_dependencies = ["base"],
)

cc_haskell_import(
  name = "add-one-so",
  dep = ":add-one-hs",
)

cc_binary(
  name = "add-one",
  srcs = ["main.c", ":add-one-so"],
  deps = ["@ghc//:threaded-rts"],
  visibility = ["//visibility:public"],
)
