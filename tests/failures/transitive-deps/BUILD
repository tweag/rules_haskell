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
    prebuilt_dependencies = ["base"],
    visibility = ["//visibility:private"],
    deps = [":lib-a"],
)

# Targets that must FAIL. These are tagged as manual so that
#
# $ bazel build //...
#
# does not fail.

haskell_library(
    # Should fail because it doesn't specify "base" explicitly.
    name = "lib-cFailure",
    srcs = ["LibC.hs"],
    prebuilt_dependencies = [],
    tags = ["manual"],
    deps = [":lib-b"],
)

haskell_library(
    name = "lib-c",
    srcs = ["LibC.hs"],
    prebuilt_dependencies = ["base"],
    deps = [":lib-b"],
)

haskell_library(
    # Should fail because it doesn't specify "lib-a" explicitly.
    name = "lib-dFailure",
    srcs = ["LibD.hs"],
    prebuilt_dependencies = ["base"],
    tags = ["manual"],
    deps = [":lib-b"],
)

haskell_library(
    name = "lib-d",
    srcs = ["LibD.hs"],
    prebuilt_dependencies = [
        "base",
        "lib-a",
    ],
    deps = [":lib-b"],
)
