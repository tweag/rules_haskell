load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_toolchain",
  "haskell_doctest_toolchain",
)
load("@io_tweag_rules_haskell//haskell:c2hs.bzl",
  "c2hs_toolchain",
)
load("//tools:mangling.bzl", "hazel_binary")

exports_files([
    "hazel.bzl",
    "BUILD.ghc",
    "paths-template.hs",
    "cc_configure_custom.bzl",
])

haskell_doctest_toolchain(
    name = "doctest",
    doctest = hazel_binary("doctest"),
)

c2hs_toolchain(
    name = "c2hs-toolchain",
    c2hs = "@c2hs//:bin",
)

cc_import(
    name = "pq-library",
    shared_library = "@postgresql//:lib",
)

cc_library(
    name = "pq",
    deps = [":pq-library"],
    hdrs = ["@postgresql//:headers"],
    strip_include_prefix = "external/postgresql/include",
    visibility = ["//visibility:public"],
)

cc_import(
    name = "sndfile-library",
    shared_library = "@libsndfile.out//:lib",
)

cc_library(
    name = "sndfile",
    deps = [":sndfile-library"],
    hdrs = ["@libsndfile.dev//:headers"],
    strip_include_prefix = "external/libsndfile.dev/include",
    visibility = ["//visibility:public"],
)

cc_import(
    name = "tag_c-library",
    shared_library = "@taglib//:lib",
)

cc_library(
    name = "tag_c",
    deps = [":tag_c-library"],
    visibility = ["//visibility:public"],
)
