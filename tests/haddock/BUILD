package(default_testonly = 1, default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_import",
  "haskell_library",
  "haskell_haddock",
)

haskell_import(name = "zlib", shared_library = "@zlib//:lib")

haskell_library(
  name = "haddock-lib-a",
  srcs = ["LibA.hs", "LibA/A.hs"],
  prebuilt_dependencies = ["base"],
)

haskell_library(
  name = "haddock-lib-b",
  srcs = ["LibB.hs"],
  deps = [":haddock-lib-a", ":zlib"],
  prebuilt_dependencies = ["base"],
  visibility = ["//visibility:public"],
)

haskell_haddock(
  name = "haddock",
  deps = [":haddock-lib-b"],
)
