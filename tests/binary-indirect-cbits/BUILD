load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_binary",
    "haskell_library",
)

haskell_binary(
    name = "binary-indirect-cbits",
    srcs = ["Main.hs"],
    linkstatic = False,
    visibility = ["//visibility:public"],
    deps = [
        "//tests/hackage:base",
        "//tests/library-with-cbits",
    ],
)

haskell_binary(
    name = "binary-indirect-cbits-partially-static",
    srcs = ["Main.hs"],
    linkstatic = False,
    deps = [
        "//tests/hackage:base",
        "//tests/library-with-cbits:library-with-cbits-static",
    ],
)

haskell_binary(
    name = "binary-indirect-cbits-fully-static",
    srcs = ["Main.hs"],
    linkstatic = True,
    deps = [
        "//tests/hackage:base",
        "//tests/library-with-cbits:library-with-cbits-static",
    ],
)

haskell_binary(
    name = "binary-two-indirect-cbits",
    srcs = ["Main2.hs"],
    linkstatic = False,
    visibility = ["//visibility:public"],
    deps = [
        "//tests/hackage:base",
        "//tests/library-with-two-cbits",
    ],
)

haskell_binary(
    name = "binary-two-indirect-cbits-partially-static",
    srcs = ["Main2.hs"],
    linkstatic = False,
    deps = [
        "//tests/hackage:base",
        "//tests/library-with-two-cbits:library-with-two-cbits-static",
    ],
)

haskell_binary(
    name = "binary-two-indirect-cbits-fully-static",
    srcs = ["Main2.hs"],
    linkstatic = True,
    deps = [
        "//tests/hackage:base",
        "//tests/library-with-two-cbits:library-with-two-cbits-static",
    ],
)
