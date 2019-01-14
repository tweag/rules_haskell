load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
    "haskell_import"
)

haskell_library (
    name = "library-with-cbits",
    srcs = ["AddOne.hsc"],
    deps = [
        "//tests/data:ourclibrary",
        "@hackage//:base"
    ],
    visibility = ["//visibility:public"],
)

haskell_library (
    name = "library-with-cbits-static",
    srcs = ["AddOne.hsc"],
    deps = [
        ":ourclibrary-static",
        "@hackage//:base",
    ],
    visibility = ["//visibility:public"],
)

cc_library(
  name = "ourclibrary-static",
  deps = [
      "//tests/data:ourclibrary",
  ],
  linkstatic = True
)

