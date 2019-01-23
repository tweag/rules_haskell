load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
    "haskell_test",
)

package(default_testonly = 1)

haskell_library(
    name = "hsc-lib",
    srcs = [
        "Bar.hsc",
        "Bar/Baz.hsc",
        "Flags.hsc",
        "Foo.hsc",
    ],
    compiler_flags = [
        "-DTHIS_IS_TRUE",
        "-optP-DTHIS_TOO_IS_TRUE",
    ],
    deps = ["//tests/hackage:base"],
)

haskell_test(
    name = "hsc",
    srcs = [
        "BinHsc.hsc",
        "Main.hs",
    ],
    visibility = ["//visibility:public"],
    deps = [
        ":hsc-lib",
        "//tests/hackage:base",
    ],
)
