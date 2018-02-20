package(default_testonly = 1)

load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_binary",
  "haskell_library",
  "haskell_cc_import",
)

haskell_cc_import(
  name = "zlib",
  shared_library = "@zlib//:lib",
  hdrs = ["@zlib.dev//:include"],
)

haskell_library(
  name = "intermediate-library",
  srcs = ["IntLib.hsc"],
  deps = [":zlib"],
  prebuilt_dependencies = ["base"],
)

haskell_library(
  name = "library-with-sysincludes",
  srcs = [
      "Lib.hs",
      "TH.hs",
  ],
  deps = [":intermediate-library"],
  prebuilt_dependencies = [
      "base",
      "template-haskell",
  ],
  visibility = ["//visibility:public"],
)
