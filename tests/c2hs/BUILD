package(default_testonly = 1)

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_c2hs",
  "haskell_library",
)

cc_import(
  name = "zlib-cc",
  hdrs = ["@zlib.dev//:include"],
  shared_library = "@zlib//:lib-single",
)

haskell_c2hs(
  name = "c2hs",
  srcs = [
    "Foo.chs",
    "Bar.chs",
  ],
  copts = ["-Iexternal/zlib.dev/include"],
  deps = [":zlib-cc"],
)

haskell_library(
  name = "c2hs-consumer",
  srcs = [":c2hs"],
  prebuilt_dependencies = [
    "base",
  ],
)
