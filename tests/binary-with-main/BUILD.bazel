load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_test",
)

package(default_testonly = 1)

haskell_test(
    name = "binary-with-main",
    srcs = ["MainIsHere.hs"],
    main_function = "MainIsHere.this",
    visibility = ["//visibility:public"],
    deps = ["//tests/hackage:base"],
)
