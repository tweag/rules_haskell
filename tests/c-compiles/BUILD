load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
    "haskell_test",
)

package(default_testonly = 1)

haskell_library(
    name = "hs-lib",
    srcs = ["Lib.hs"],
    deps = [
        "//tests/data:ourclibrary",
        "//tests/hackage:base",
    ],
    linkstatic = select({
        "@bazel_tools//src/conditions:windows": True,
        "//conditions:default": False,
    }),
)

haskell_test(
    name = "c-compiles",
    srcs = ["Main.hs"],
    visibility = ["//visibility:public"],
    deps = [
        ":hs-lib",
        "//tests/hackage:base",
    ],
)
