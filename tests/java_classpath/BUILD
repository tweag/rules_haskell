package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_binary",
)

haskell_binary(
    name = "java_classpath",
    srcs = ["Main.hs"],
    visibility = ["//visibility:public"],
    deps = [
        "//tests:base",
        "//tests:template-haskell",
        "@org_apache_spark_spark_core_2_10//jar",
    ],
)
