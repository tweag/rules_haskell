package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_binary",
    "haskell_library",
)

haskell_library(
    name = "hsc-lib",
    srcs = [
        "Bar.hsc",
        "Bar/Baz.hsc",
        "Foo.hsc",
    ],
    prebuilt_dependencies = ["base"],
)

haskell_binary(
    name = "hsc",
    srcs = [
        "BinHsc.hsc",
        "Main.hs",
    ],
    prebuilt_dependencies = ["base"],
    visibility = ["//visibility:public"],
    deps = [":hsc-lib"],
)
