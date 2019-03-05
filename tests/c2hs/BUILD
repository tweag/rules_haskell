load("@io_tweag_rules_haskell//haskell:c2hs.bzl", "c2hs_library")
load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
)

package(default_testonly = 1)

c2hs_library(
    name = "foo",
    srcs = ["src/Foo/Foo.chs"],
    src_strip_prefix = "src",
    tags = [
        "requires_c2hs",
        "requires_zlib",
    ],
    deps = ["@zlib.dev//:zlib"],
)

c2hs_library(
    name = "bar",
    srcs = ["Bar.chs"],
    tags = ["requires_c2hs"],
    deps = [":foo"],
)

haskell_library(
    name = "c2hs",
    srcs = [
        ":bar",
        ":foo",
        "@c2hs_repo//:baz",
    ],
    tags = ["requires_c2hs"],
    deps = ["//tests/hackage:base"],
)
