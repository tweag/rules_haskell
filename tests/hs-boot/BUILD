package(default_testonly = 1)

load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_binary",
)

haskell_library(
  name = "hs-boot-lib",
  srcs = ["A.hs", "A.hs-boot", "B.hs"],
  prebuilt_dependencies = ["base"],
  visibility = ["//visibility:public"],
)

haskell_binary(
  name = "hs-boot",
  srcs = ["MA.hs", "MA.hs-boot", "MB.hs", "Main.hs"],
  deps = [":hs-boot-lib"],
  prebuilt_dependencies = ["base"],
  visibility = ["//visibility:public"],
)
