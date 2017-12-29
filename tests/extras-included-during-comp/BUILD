package(default_testonly = 1)

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_binary",
)

haskell_binary(
  name = "extras-included-during-comp",
  srcs = ["Main.hs"],
  external_deps = ["includes/main_definition"],
  prebuilt_dependencies = ["base"],
  visibility = ["//visibility:public"],
)
