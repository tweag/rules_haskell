load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_binary",
    "haskell_library",
)

haskell_binary(
    name = "binary-indirect-cbits",
    srcs = ["Main.hs"],
    linkstatic = False,
    deps = [
        "//tests/library-with-cbits",
        "@hackage//:base",
    ],
)

haskell_binary(
    name = "binary-indirect-cbits-partially-static",
    srcs = ["Main.hs"],
    linkstatic = False,
    deps = [
        "//tests/library-with-cbits:library-with-cbits-static",
        "@hackage//:base",
    ],
)

haskell_binary(
    name = "binary-indirect-cbits-fully-static",
    srcs = ["Main.hs"],
    linkstatic = True,
    deps = [
        "//tests/library-with-cbits:library-with-cbits-static",
        "@hackage//:base",
    ],
)
