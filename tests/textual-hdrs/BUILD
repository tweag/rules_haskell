load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_test",
)

package(default_testonly = 1)

haskell_test(
    name = "textual-hdrs",
    srcs = [
        "Main.hs",
        "include/main_definition.h",
    ],
    compiler_flags = ["-Itests/textual-hdrs/include"],
    visibility = ["//visibility:public"],
    deps = ["//tests/hackage:base"],
)
