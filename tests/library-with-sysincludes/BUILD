package(default_testonly = 1)

load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_binary",
  "haskell_library",
  "haskell_cc_import",
)

# A locally-defined replica of @zlib.dev//:zlib.
# Since that shared library lives in another package, we must
# use an absolute path for strip_include_prefix.
haskell_cc_import(
  name = "zlib",
  shared_library = "@zlib//:lib",
  hdrs = ["@zlib.dev//:include"],
  strip_include_prefix = "/external/zlib.dev/include",
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

# Replicate the above example, but use the externall-defined haskell_cc_import
# rule.
haskell_library(
  name = "intermediate-library-other",
  srcs = ["IntLib.hsc"],
  deps = ["@zlib.dev//:zlib"],
  prebuilt_dependencies = ["base"],
)

haskell_library(
  name = "library-with-sysincludes-other",
  srcs = [
      "Lib.hs",
      "TH.hs",
  ],
  deps = [":intermediate-library-other"],
  prebuilt_dependencies = [
      "base",
      "template-haskell",
  ],
  visibility = ["//visibility:public"],
)
