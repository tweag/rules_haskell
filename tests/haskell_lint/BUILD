package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_binary",
    "haskell_library",
    "haskell_lint",
    "haskell_test",
)

haskell_library(
    name = "lib-a",
    srcs = ["Foo.hs"],
    prebuilt_dependencies = ["base"],
    visibility = ["//visibility:public"],
)

haskell_library(
    name = "lib-b",
    srcs = ["Bar.hs"],
    prebuilt_dependencies = ["base"],
    visibility = ["//visibility:public"],
    deps = [":lib-a"],
)

haskell_lint(
    name = "lint-lib-b",
    visibility = ["//visibility:public"],
    deps = [":lib-b"],
)

haskell_binary(
    name = "bin",
    srcs = ["Main.hs"],
    prebuilt_dependencies = ["base"],
    visibility = ["//visibility:public"],
    deps = [":lib-a"],
)

haskell_lint(
    name = "lint-bin",
    visibility = ["//visibility:public"],
    deps = [":bin"],
)
