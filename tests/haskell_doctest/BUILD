load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_doctest",
    "haskell_library",
    "haskell_test",
)

package(default_testonly = 1)

haskell_library(
    name = "lib-a",
    srcs = ["Foo.hs"],
    tags = ["dont_test_when_bindist"],
    deps = [
        "//tests/data:ourclibrary",
        "//tests/hackage:base",
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
    tags = ["dont_test_when_bindist"],
    deps = [
        ":lib-a",
        "//tests/hackage:base",
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
    tags = ["dont_test_when_bindist"],
    visibility = ["//visibility:public"],
    deps = [":lib-b"],
)

haskell_doctest(
    name = "doctest-lib",
    modules = ["Bar"],  # exclude Baz and succeed
    tags = ["dont_test_when_bindist"],
    visibility = ["//visibility:public"],
    deps = [":lib-b"],
)

haskell_test(
    name = "bin",
    srcs = ["Main.hs"],
    tags = ["dont_test_when_bindist"],
    deps = [
        ":lib-a",
        "//tests/hackage:base",
    ],
)

haskell_doctest(
    name = "doctest-bin",
    tags = ["dont_test_when_bindist"],
    visibility = ["//visibility:public"],
    deps = [":bin"],
)
