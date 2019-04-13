# Tests correct linking of haskell packages that were created
# in a different bazel repository, e.g. with hazel.

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_test",
)

haskell_test(
    name = "external-haskell-repository",
    srcs = ["Main.hs"],
    visibility = ["//visibility:public"],
    deps = [
        "//tests/hackage:base",
        "@haskell_package_repository_dummy//:library-with-cbits",
    ],
)
