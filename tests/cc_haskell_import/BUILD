load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "cc_haskell_import",
    "haskell_library",
)

package(default_testonly = 1)

haskell_library(
    name = "hs-lib-a",
    srcs = ["LibA.hs"],
    deps = [
        "//tests/data:ourclibrary",
        "//tests/hackage:base",
    ],
)

haskell_library(
    name = "hs-lib-b",
    srcs = ["LibB.hs"],
    deps = [
        ":hs-lib-a",
        "//tests/hackage:base",
    ],
)

cc_haskell_import(
    name = "hs-lib-b.so",
    dep = ":hs-lib-b",
    visibility = ["//tests:__subpackages__"],
)

cc_binary(
    name = "cc-bin",
    srcs = [
        "main.c",
        ":hs-lib-b.so",
    ],
    visibility = ["//tests:__subpackages__"],
    deps = ["@ghc//:threaded-rts"],
)

# We go one step further and use the Haskell library from above
# to build a static .so which is then loaded with a Python script
# and calls the Haskell function constructed from GHC C FFI.

# shared library which python will dlopen
cc_binary(
    name = "hs-lib-b-wrapped.so",
    srcs = [
        ":hs-lib-b.so",
    ],
    linkshared = 1,
    linkstatic = 1,
    visibility = ["//tests:__subpackages__"],
    deps = ["@ghc//:threaded-rts"],
)

# just dlopens hb-lib-b-wrapped.so and prints it
py_binary(
    name = "python_add_one",
    srcs = ["python_add_one.py"],
    data = [
        ":hs-lib-b-wrapped.so",
    ],
    default_python_version = "PY3",
    srcs_version = "PY3ONLY",
    visibility = ["//tests:__subpackages__"],
    deps = ["@bazel_tools//tools/python/runfiles"],
)
