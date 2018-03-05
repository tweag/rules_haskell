package(default_testonly = 1)

load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_binary",
)

haskell_binary(
  name = "bin1",
  srcs = ["bin1.hs"],
  main_file = "bin1.hs",
  prebuilt_dependencies = ["base"],
  visibility = ["//visibility:public"],
)

haskell_binary(
  name = "binary-with-data",
  srcs = ["bin2.hs"],
  main_file = "bin2.hs",
  data = [":bin1"],
  args = ["$(location :bin1)"],
  prebuilt_dependencies = ["base", "process"],
  visibility = ["//visibility:public"],
)
