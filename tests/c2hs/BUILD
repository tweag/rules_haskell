package(default_testonly = 1)

load("@io_tweag_rules_haskell//haskell:c2hs.bzl", "c2hs_library")
load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_cc_import",
    "haskell_library",
)

haskell_cc_import(
    name = "zlib",
    hdrs = ["@zlib.dev//:include"],
    shared_library = "@zlib//:lib",
    strip_include_prefix = "/external/zlib.dev/include",
    visibility = ["@c2hs_repo//:__pkg__"],
)

c2hs_library(
    name = "foo",
    srcs = ["src/Foo/Foo.chs"],
    src_strip_prefix = "src",
    deps = [":zlib"],
)

c2hs_library(
    name = "bar",
    srcs = ["Bar.chs"],
    deps = [":foo"],
)

haskell_library(
    name = "c2hs",
    srcs = [
        ":bar",
        ":foo",
        "@c2hs_repo//:baz",
    ],
    deps = ["//tests:base"],
)
