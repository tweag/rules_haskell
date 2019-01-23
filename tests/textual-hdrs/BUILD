load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_import_boot",
    "haskell_test",
)

package(default_testonly = 1)

haskell_import_boot()

haskell_test(
    name = "textual-hdrs",
    srcs = [
        "Main.hs",
        "include/main_definition.h",
    ],
    compiler_flags = ["-Itests/textual-hdrs/include"],
    visibility = ["//visibility:public"],
    deps = [":base"],
)
