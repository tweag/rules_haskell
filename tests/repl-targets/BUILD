load("@io_tweag_rules_haskell//haskell:c2hs.bzl", "c2hs_library")
load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
    "haskell_test",
)

package(default_testonly = 1)

genrule(
    name = "codegen",
    outs = [
        "Gen.hs",
    ],
    cmd = """
  echo "module Gen (gen) where" >> $(location :Gen.hs)
  echo "gen :: String" >> $(location :Gen.hs)
  echo "gen = \\"gen\\"" >> $(location :Gen.hs)
""",
)

c2hs_library(
    name = "chs",
    srcs = ["Chs.chs"],
    tags = ["requires_c2hs"],
)

haskell_library(
    name = "hs-lib",
    srcs = [
        "Foo.hs",
        "Hsc.hsc",
        ":chs",
        ":codegen",
    ],
    tags = [
        "requires_hackage",
        "requires_zlib",
    ],
    visibility = ["//visibility:public"],
    deps = [
        "//tests/data:ourclibrary",
        "//tests/hackage:array",
        "//tests/hackage:base",
        "@zlib",
    ],
)

haskell_library(
    name = "hs-lib-bad",
    srcs = [
        "Bad.hs",
    ],
    tags = [
        "manual",
        "requires_zlib",
    ],
    visibility = ["//visibility:public"],
    deps = [
        "//tests/data:ourclibrary",
        "//tests/hackage:base",
        "@hackage//:array",
        "@zlib",
    ],
)

haskell_library(
    name = "QuuxLib",
    srcs = ["QuuxLib.hs"],
    deps = ["//tests/hackage:base"],
)

haskell_test(
    name = "hs-bin",
    srcs = ["Quux.hs"],
    visibility = ["//visibility:public"],
    deps = [
        ":QuuxLib",
        "//tests/hackage:base",
    ],
)
