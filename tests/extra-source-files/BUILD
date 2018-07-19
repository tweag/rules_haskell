package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
)

haskell_library(
    name = "extra-source-files",
    srcs = [
        "Foo.hs",
        "FooTH.hs",
    ],
    extra_srcs = [
        "file.txt",
    ],
    prebuilt_dependencies = [
        "base",
        "template-haskell",
    ],
)
