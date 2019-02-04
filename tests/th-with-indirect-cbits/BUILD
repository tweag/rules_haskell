load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
    "haskell_test",
)

cc_library(
    name = "cc-lib-indirect",
    srcs = ["cc_lib_indirect.c"],
)

cc_library(
    name = "cc-lib-direct",
    srcs = ["cc_lib_direct.c"],
    deps = [
        ":cc-lib-indirect",
    ],
)

haskell_library(
    name = "hs-lib-indirect",
    srcs = ["HsLibIndirect.hs"],
    deps = [
        ":cc-lib-direct",
        "//tests/hackage:base",
    ],
)

haskell_library(
    name = "hs-lib-direct",
    srcs = ["HsLibDirect.hs"],
    deps = [
        ":hs-lib-indirect",
        "//tests/hackage:base",
        "//tests/hackage:template-haskell",
    ],
)

haskell_test(
    name = "binary-with-th-with-indirect-cbits",
    srcs = ["Main.hs"],
    deps = [
        ":hs-lib-direct",
        "//tests/hackage:base",
    ],
)

haskell_library(
    name = "library-with-th-with-indirect-cbits",
    srcs = ["Library.hs"],
    deps = [
        ":hs-lib-direct",
        "//tests/hackage:base",
    ],
)

test_suite(
    name = "th-with-indirect-cbits",
    tests = [
        ":binary-with-th-with-indirect-cbits",
    ],
)
