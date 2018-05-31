package(default_testonly = 1)

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_cc_import",
)

haskell_cc_import(
  name = "zlib",
  shared_library = "@zlib//:lib",
  hdrs = ["@zlib.dev//:include"],
  strip_include_prefix = "/external/zlib.dev/include",
)

haskell_library(
  name = "c2hs",
  srcs = [
    "Foo.chs",
    "Bar.chs",
  ],
  deps = [
    ":zlib",
  ],
  prebuilt_dependencies = [
    "base",
  ],
)
