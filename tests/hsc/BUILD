load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_binary",
)

haskell_library(
  name = "hsc-lib",
  hscs = ["Test.hsc"],
  prebuilt_dependencies = ["base"],
)

# https://github.com/tweag/rules_haskell/issues/13
haskell_binary(
  name = "hsc",
  srcs = ["Main.hs"],
  prebuilt_dependencies = ["base"],
  deps = [":hsc-lib"],
  visibility = ["//visibility:public"],
)
