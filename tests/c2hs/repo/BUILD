load("@io_tweag_rules_haskell//haskell:c2hs.bzl", "c2hs_library")

package(default_testonly = 1)

c2hs_library(
    name = "baz",
    srcs = ["Baz.chs"],
    visibility = ["//visibility:public"],
    deps = ["@zlib.dev//:zlib"],
)
