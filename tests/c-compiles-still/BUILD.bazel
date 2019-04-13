load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
)

package(default_testonly = 1)

haskell_library(
    name = "foo",
    srcs = ["Foo.hs"],
    deps = [
        "//tests/data:ourclibrary",
        "//tests/hackage:base",
    ],
)
