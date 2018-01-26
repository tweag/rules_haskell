package(default_testonly = 1)

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_binary",
)

haskell_library(
  name = "hsc-lib",
  srcs = [
      "Test0.hsc",
      "Test1.hsc",
  ],
  prebuilt_dependencies = ["base"],
)

haskell_binary(
  name = "hsc",
  srcs = ["Main.hs", "BinHsc.hsc"],
  prebuilt_dependencies = ["base"],
  deps = [":hsc-lib"],
  visibility = ["//visibility:public"],
)
