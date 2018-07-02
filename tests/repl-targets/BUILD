package(default_testonly = 1)

load("@io_tweag_rules_haskell//haskell:haskell.bzl",
     "haskell_library",
     "haskell_binary",
     "haskell_cc_import",
)

cc_library(
  name = "cbits",
  srcs = ["cbits/Lib.c"],
)

haskell_cc_import(
  name = "zlib",
  shared_library = "@zlib//:lib",
  hdrs = ["@zlib.dev//:include"],
  strip_include_prefix = "/external/zlib.dev/include",
)

haskell_library(
  name = "hs-lib",
  srcs = [
    "Foo.hs",
    "Hsc.hsc",
    "Chs.chs",
  ],
  prebuilt_dependencies = ["base", "array"],
  deps = [
    ":cbits",
    ":zlib",
  ],
  visibility = ["//visibility:public"],
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
  deps = [":QuuxLib"],
  prebuilt_dependencies = ["base"],
  visibility = ["//visibility:public"],
)
