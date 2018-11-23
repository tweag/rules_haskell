load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_binary",
)

package(
    default_testonly = 1,
    default_visibility = ["//visibility:public"],
)

haskell_binary(
    name = "binary-with-link-flags",
    srcs = ["Main.hs"],
    compiler_flags = ["-threaded"],
    visibility = ["//visibility:public"],
    deps = ["@hackage//:base"],
)
