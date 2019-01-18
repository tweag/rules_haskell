load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_import",
    "haskell_test",
)

package(default_testonly = 1)

haskell_import(package = "base")

haskell_test(
    name = "binary-custom-main",
    srcs = ["foo.hs"],
    visibility = ["//visibility:public"],
    deps = [":base"],
)
