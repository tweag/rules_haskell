load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_test",
)

package(default_testonly = 1)

haskell_test(
    name = "java_classpath",
    srcs = ["Main.hs"],
    visibility = ["//visibility:public"],
    deps = [
        "//tests/hackage:base",
        "//tests/hackage:template-haskell",
        "@org_apache_spark_spark_core_2_10//jar",
    ],
)
