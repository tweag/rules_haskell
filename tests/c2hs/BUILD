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
)

c2hs_library(
    name = "foo",
    srcs = ["Foo.chs"],
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
    ],
    prebuilt_dependencies = ["base"],
)
