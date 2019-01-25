load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
    "haskell_test",
)

package(default_testonly = 1)

# Depends on system zlib imported using cc_library.
haskell_library(
    name = "hs-lib-cc",
    srcs = ["HsLib.hs"],
    tags = ["requires_zlib"],
    deps = [
        "//tests/hackage:base",
        "@zlib.dev//:cc-zlib",
    ],
)

# Depends on sysmte zlib imported using haskell_cc_import.
haskell_library(
    name = "hs-lib-import",
    srcs = ["HsLib.hs"],
    tags = ["requires_zlib"],
    deps = [
        "//tests/hackage:base",
        "@zlib.dev//:zlib",
    ],
)

haskell_test(
    name = "binary-with-indirect-sysdeps-cc",
    srcs = ["Main.hs"],
    tags = ["requires_zlib"],
    deps = [
        ":hs-lib-cc",
        "//tests/hackage:base",
    ],
)

haskell_test(
    name = "binary-with-indirect-sysdeps-import",
    srcs = ["Main.hs"],
    tags = ["requires_zlib"],
    deps = [
        ":hs-lib-import",
        "//tests/hackage:base",
    ],
)

test_suite(
    name = "binary-with-indirect-sysdeps",
    tags = ["requires_zlib"],
    tests = [
        ":binary-with-indirect-sysdeps-cc",
        ":binary-with-indirect-sysdeps-import",
    ],
)
