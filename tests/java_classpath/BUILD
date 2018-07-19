package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_binary",
)

haskell_binary(
    name = "java_classpath",
    srcs = ["Main.hs"],
    prebuilt_dependencies = [
        "base",
        "template-haskell",
    ],
    visibility = ["//visibility:public"],
    deps = ["@org_apache_spark_spark_core_2_10//jar"],
)
