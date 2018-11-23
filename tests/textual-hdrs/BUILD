load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_binary",
)

package(default_testonly = 1)

haskell_binary(
    name = "textual-hdrs",
    srcs = [
        "Main.hs",
        "include/main_definition.h",
    ],
    compiler_flags = ["-Itests/textual-hdrs/include"],
    visibility = ["//visibility:public"],
    deps = ["@hackage//:base"],
)
