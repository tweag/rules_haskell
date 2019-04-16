load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_binary",
)

haskell_binary(
    name = "coverage-report-renderer",
    srcs = ["Main.hs"],
    visibility = ["//visibility:public"],
    deps = [
        "@hackage//:MissingH",
        "@hackage//:base",
        "@hackage//:cmdargs",
        "@hackage//:directory",
        "@hackage//:filepath",
        "@hackage//:hxt",
        "@hackage//:hxt-xpath",
        "@hackage//:listsafe",
    ],
)
