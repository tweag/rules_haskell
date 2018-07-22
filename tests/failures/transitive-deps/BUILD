package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
)

haskell_library(
    name = "lib-a",
    srcs = ["LibA.hs"],
    deps = ["//tests:base"],
)

haskell_library(
    name = "lib-b",
    srcs = ["LibB.hs"],
    visibility = ["//visibility:private"],
    deps = [
        ":lib-a",
        "//tests:base",
    ],
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
    tags = ["manual"],
    deps = [":lib-b"],
)

haskell_library(
    name = "lib-c",
    srcs = ["LibC.hs"],
    deps = [
        ":lib-b",
        "//tests:base",
    ],
)

haskell_library(
    # Should fail because it doesn't specify "lib-a" explicitly.
    name = "lib-dFailure",
    srcs = ["LibD.hs"],
    tags = ["manual"],
    deps = [
        ":lib-b",
        "//tests:base",
    ],
)

haskell_library(
    name = "lib-d",
    srcs = ["LibD.hs"],
    deps = [
        ":lib-a",
        ":lib-b",
        "//tests:base",
    ],
)
