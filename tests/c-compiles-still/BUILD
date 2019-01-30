load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
)

package(default_testonly = 1)

haskell_library(
    name = "foo",
    srcs = ["Foo.hs"],
    linkstatic = select({
        "@bazel_tools//src/conditions:windows": True,
        "//conditions:default": False,
    }),
    deps = [
        "//tests/data:ourclibrary",
        "//tests/hackage:base",
    ],
)
