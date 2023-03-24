# This tests the asterius stack_snapshot which makes use of the toolchain_libraries attribute.
# The :bin example can be compiled with both the asterius toolchain and the regular one.
# We make use of aliases to select the correct libraries depending on the target platform.

load("//tests:asterius/asterius_tests_utils.bzl", "tags")
load(
    "@rules_haskell//haskell:defs.bzl",
    "haskell_test",
    "haskell_toolchain_library",
)
load(
    "@rules_haskell//haskell/asterius:defs.bzl",
    "ahc_dist",
    "asterius_test",
    "asterius_webpack",
)

# base is a target platform for all toolchains.
haskell_toolchain_library(name = "base")

# integer-gmp and xhtml are not toolchain libraries for asterius
[
    haskell_toolchain_library(
        name = lib,
        target_compatible_with = select({
            "@platforms//cpu:wasm32": ["@platforms//:incompatible"],
            "//conditions:default": [],
        }),
    )
    for lib in [
        "integer-gmp",
        "xhtml",
    ]
]

# integer-simple is only a toolchain_library for asterius
haskell_toolchain_library(
    name = "integer-simple",
    target_compatible_with = ["@platforms//cpu:wasm32"],
)

# xhtml is a toolchain library for ghc, but for asterius we recover it
# from a custom stack snapshot.
alias(
    name = "xhtml_alias",
    actual = select({
        "@rules_haskell//haskell/asterius": "@stackage_asterius//:xhtml",
        "//conditions:default": ":xhtml",
    }),
)

# Asterius is not compatible with integer-gmp and must use integer-simple
alias(
    name = "integer_alias",
    actual = select({
        "@rules_haskell//haskell/asterius": ":integer-simple",
        "//conditions:default": ":integer-gmp",
    }),
)

haskell_test(
    name = "bin",
    srcs = ["main.hs"],
    tags = tags,
    deps = [
        ":base",
        ":integer_alias",
        ":xhtml_alias",
    ],
)

entry_point = "subfolder/ahc_dist_bin.mjs"

ahc_dist(
    name = "ahc_dist_bin",
    testonly = 1,
    dep = ":bin",
    entry_point = entry_point,
    tags = tags,
)

asterius_test(
    name = "asterius_bin",
    ahc_dist_dep = ":ahc_dist_bin",
    entry_point = entry_point,
    tags = tags,
)

asterius_webpack(
    name = "asterius_bundle",
    testonly = 1,
    ahc_dist_dep = ":ahc_dist_bin",
    entry_point = entry_point,
    tags = tags,
)
