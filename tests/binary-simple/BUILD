load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_test",
)

package(default_testonly = 1)

haskell_test(
    name = "binary-simple",
    srcs = ["Main.hs"],
    expected_covered_expressions_percentage = 100,
    tags = ["coverage-compatible"],
    visibility = ["//visibility:public"],
    deps = ["//tests/hackage:base"],
)
