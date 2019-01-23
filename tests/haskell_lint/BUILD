load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_import_boot",
    "haskell_library",
    "haskell_lint",
    "haskell_test",
)

package(default_testonly = 1)

haskell_import_boot()

haskell_library(
    name = "lib-a",
    srcs = ["Foo.hs"],
    visibility = ["//visibility:public"],
    deps = [":base"],
)

haskell_library(
    name = "lib-b",
    srcs = ["Bar.hs"],
    visibility = ["//visibility:public"],
    deps = [
        ":lib-a",
        ":base",
    ],
)

haskell_lint(
    name = "lint-lib-b",
    visibility = ["//visibility:public"],
    deps = [":lib-b"],
)

haskell_test(
    name = "bin",
    srcs = ["Main.hs"],
    visibility = ["//visibility:public"],
    deps = [
        ":lib-a",
        ":base",
    ],
)

haskell_lint(
    name = "lint-bin",
    visibility = ["//visibility:public"],
    deps = [":bin"],
)
