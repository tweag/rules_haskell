load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
    "haskell_test",
)

package(default_testonly = 1)

genrule(
    name = "gen-A-boot",
    srcs = ["A.hs-boot.in"],
    outs = ["srcs/A.hs-boot"],
    cmd = "cp $< $@",
)

genrule(
    name = "gen-A",
    srcs = ["A.hs.in"],
    outs = ["srcs/A.hs"],
    cmd = "cp $< $@",
)

haskell_library(
    name = "hs-boot-lib",
    srcs = [
        "srcs/B.hs",
        ":gen-A",
        ":gen-A-boot",
    ],
    src_strip_prefix = "srcs",
    visibility = ["//visibility:public"],
    deps = ["//tests/hackage:base"],
)

haskell_test(
    name = "hs-boot",
    srcs = [
        "MA.hs",
        "MA.hs-boot",
        "MB.hs",
        "Main.hs",
    ],
    visibility = ["//visibility:public"],
    deps = [
        ":hs-boot-lib",
        "//tests/hackage:base",
    ],
)
