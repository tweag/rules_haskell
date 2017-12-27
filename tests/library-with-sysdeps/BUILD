package(default_testonly = 1)

load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_binary",
  "haskell_import",
  "haskell_library",
)

haskell_import(name = "zlib", shared_library = "@zlib//:lib")

haskell_library(
  name = "library-with-sysdeps",
  srcs = ["Lib.hs"],
  deps = [":zlib"],
  prebuilt_dependencies = ["base"],
  visibility = ["//visibility:public"],
)

haskell_binary(
  name = "bin",
  srcs = ["Main.hs"],
  deps = [":library-with-sysdeps"],
  prebuilt_dependencies = ["base"],
)
