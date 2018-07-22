package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_binary",
    "haskell_doctest",
    "haskell_library",
)

cc_library(
    name = "cbits",
    srcs = ["cbits.c"],
)

haskell_library(
    name = "lib-a",
    srcs = ["Foo.hs"],
    deps = [
        ":cbits",
        "//tests:base",
        "@zlib.dev//:zlib",
    ],
)

haskell_library(
    name = "lib-b",
    srcs = [
        "Bar.hs",
        "Baz.hs",
        "Quux.hsc",
    ],
    deps = [
        ":lib-a",
        "//tests:base",
    ],
)

haskell_doctest(
    name = "doctest-lib-all-fail",
    tags = ["manual"],  # must FAIL
    visibility = ["//visibility:public"],
    deps = [":lib-b"],
)

haskell_doctest(
    name = "doctest-lib-all-success",
    doctest_flags = ["-DMAGIC_DOCTEST_THING"],
    visibility = ["//visibility:public"],
    deps = [":lib-b"],
)

haskell_doctest(
    name = "doctest-lib",
    modules = ["Bar"],  # exclude Baz and succeed
    visibility = ["//visibility:public"],
    deps = [":lib-b"],
)

haskell_binary(
    name = "bin",
    srcs = ["Main.hs"],
    deps = [
        ":lib-a",
        "//tests:base",
    ],
)

haskell_doctest(
    name = "doctest-bin",
    visibility = ["//visibility:public"],
    deps = [":bin"],
)
