package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_binary"
)

haskell_binary(
  name = "hello-world",
  srcs = ["Main.hs"],
  deps = ["@examples//hello-lib"]
)
