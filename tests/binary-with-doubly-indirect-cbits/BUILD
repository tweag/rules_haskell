load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_test",
)

haskell_test(
    name = "binary-with-doubly-indirect-cbits",
    srcs = ["Main.hs"],
    linkstatic = False,
    deps = [
        "//tests/binary-with-doubly-indirect-cbits/lib-direct:value",
        "@hackage//:base",
    ],
)
