package(default_testonly = 1)

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
)

haskell_library(
  name = "lib-a",
  srcs = ["LibA.hs"],
  prebuilt_dependencies = ["base"],
)

haskell_library(
  name = "lib-b",
  srcs = ["LibB.hs"],
  deps = [":lib-a"],
  prebuilt_dependencies = ["base"],
)

haskell_library(
  # Should fail because it doesn't specify "base" explicitly.
  name = "lib-c",
  srcs = ["LibC.hs"],
  deps = [":lib-b"],
  prebuilt_dependencies = [],
)

haskell_library(
  # Should fail because it doesn't specify "lib-a" explicitly.
  name = "lib-d",
  srcs = ["LibD.hs"],
  deps = [":lib-b"],
  prebuilt_dependencies = ["base"],
)
