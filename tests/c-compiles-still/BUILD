package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
)

haskell_library(
    name = "foo",
    srcs = ["Foo.hs"],
    prebuilt_dependencies = ["base"],
    deps = ["//tests/c-compiles:c-lib"],
)
