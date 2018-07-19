package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_binary",
)

haskell_binary(
    name = "textual-hdrs",
    srcs = [
        "Main.hs",
        "include/main_definition.h",
    ],
    compiler_flags = ["-Itests/textual-hdrs/include"],
    prebuilt_dependencies = ["base"],
    visibility = ["//visibility:public"],
)
