package(default_testonly = 1, default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_haddock",
)

haskell_library(
  name = "haddock-lib-a",
  srcs = ["LibA.hs", "LibA/A.hs"],
  prebuilt_dependencies = ["base"],
)

# TODO: Add system library to deps. I can't do it with something like
# zlib because bazel freaks out that zlib is a non-test dependency
# using test toolchain. I have checked once that it works by unsetting
# testonly in tests/BUILD but it seems like a non-solution.
haskell_library(
  name = "haddock-lib-b",
  srcs = ["LibB.hs"],
  deps = [":haddock-lib-a"],
  prebuilt_dependencies = ["base"],
  visibility = ["//visibility:public"],
)

haskell_haddock(
  name = "haddock",
  deps = [":haddock-lib-b"],
)
