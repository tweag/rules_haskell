load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_binary",
    "haskell_library",
)

package(default_testonly = 1)

haskell_library(
    name = "hsc-lib",
    srcs = [
        "Bar.hsc",
        "Bar/Baz.hsc",
        "Foo.hsc",
    ],
    deps = ["@hackage//:base"],
)

haskell_binary(
    name = "hsc",
    srcs = [
        "BinHsc.hsc",
        "Main.hs",
    ],
    visibility = ["//visibility:public"],
    deps = [
        ":hsc-lib",
        "@hackage//:base",
    ],
)
