package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
)

haskell_library(
  name = 'test-lib',
  srcs = [
    'TestLib.hs',
  ],
  deps = ["//tests/test-sublib"],
  prebuiltDeps = [
    "base",
  ],
)
