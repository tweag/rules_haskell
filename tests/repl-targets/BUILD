package(default_testonly = 1)

load("@io_tweag_rules_haskell//haskell:c2hs.bzl", "c2hs_library")
load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_binary",
    "haskell_cc_import",
    "haskell_library",
)

cc_library(
    name = "cbits",
    srcs = ["cbits/Lib.c"],
)

haskell_cc_import(
    name = "zlib",
    hdrs = ["@zlib.dev//:include"],
    shared_library = "@zlib//:lib",
    strip_include_prefix = "/external/zlib.dev/include",
)

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
)

haskell_library(
    name = "hs-lib",
    srcs = [
        "Foo.hs",
        "Hsc.hsc",
        ":chs",
        ":codegen",
    ],
    prebuilt_dependencies = [
        "base",
        "array",
    ],
    visibility = ["//visibility:public"],
    deps = [
        ":cbits",
        ":zlib",
    ],
)

haskell_library(
    name = "QuuxLib",
    srcs = ["QuuxLib.hs"],
    prebuilt_dependencies = ["base"],
)

haskell_binary(
    name = "hs-bin",
    srcs = ["Quux.hs"],
    main_file = "Quux.hs",
    prebuilt_dependencies = ["base"],
    visibility = ["//visibility:public"],
    deps = [":QuuxLib"],
)
