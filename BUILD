load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_toolchain",
  "haskell_doctest_toolchain",
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
