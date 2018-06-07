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
  visibility = ["//visibility:private"],
  prebuilt_dependencies = ["base"],
)

## Targets that must FAILS
# tagged as manual so bazel build //... do not fail

haskell_library(
  # Should fail because it doesn't specify "base" explicitly.
  name = "lib-cFailure",
  srcs = ["LibC.hs"],
  deps = [":lib-b"],
  prebuilt_dependencies = [],
  tags = ["manual"],
)

haskell_library(
  name = "lib-c",
  srcs = ["LibC.hs"],
  deps = [":lib-b"],
  prebuilt_dependencies = ["base"],
)

haskell_library(
  # Should fail because it doesn't specify "lib-a" explicitly.
  name = "lib-dFailure",
  srcs = ["LibD.hs"],
  deps = [":lib-b"],
  prebuilt_dependencies = ["base"],
  tags = ["manual"],
)

haskell_library(
  name = "lib-d",
  srcs = ["LibD.hs"],
  deps = [":lib-b"],
  prebuilt_dependencies = ["base", "lib-a"],
)