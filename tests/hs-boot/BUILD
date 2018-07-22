package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_binary",
    "haskell_library",
)

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
    deps = ["//tests:base"],
)

haskell_binary(
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
        "//tests:base",
    ],
)
