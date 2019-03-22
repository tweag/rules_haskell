# This file declares constraint values for each platform supported by
# GHC. These rules follow the GHC naming convention, for example,
# //haskell/platform:linux and //haskell/platform:x86_64. See the
# config.guess in any GHC source distribution for possible platforms.
#
# These can be used in select expressions to choose platform-specifc
# sources and dependencies.

load(":list.bzl", "declare_config_settings")

declare_config_settings()

constraint_value(
    name = "nixpkgs",
    constraint_setting = "@bazel_tools//tools/cpp:cc_compiler",
    visibility = ["//visibility:public"],
)

platform(
    name = "linux_x86_64_nixpkgs",
    constraint_values = [
        # XXX using the platform names defined here results in a graph
        # cycle for some reason.
        "@bazel_tools//platforms:x86_64",
        "@bazel_tools//platforms:linux",
        ":nixpkgs",
    ],
    visibility = ["//visibility:public"],
)

platform(
    name = "darwin_x86_64_nixpkgs",
    constraint_values = [
        "@bazel_tools//platforms:x86_64",
        "@bazel_tools//platforms:osx",
        ":nixpkgs",
    ],
    visibility = ["//visibility:public"],
)
