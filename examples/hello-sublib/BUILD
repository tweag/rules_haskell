package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
)

haskell_library(
  name = 'hello-sublib',
  srcs = glob(['**/*.hs']),
  deps = ["@examples//hello-subsublib"]
)
