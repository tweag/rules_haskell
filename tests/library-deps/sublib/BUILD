load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
)

package(default_testonly = 1)

haskell_library(
    name = "sublib",
    srcs = ["TestSubLib.hs"],
    linkstatic = select({
        "@bazel_tools//src/conditions:windows": True,
        "//conditions:default": False,
    }),
    visibility = ["//visibility:public"],
    deps = [
        ":sublib-c",
        "//tests/hackage:base",
    ],
)

cc_library(
    name = "sublib-c",
    srcs = ["sublib-c.c"],
)
