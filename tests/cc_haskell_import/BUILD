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
        "@hackage//:base",
    ],
)

haskell_library(
    name = "hs-lib-b",
    srcs = ["LibB.hs"],
    deps = [
        ":hs-lib-a",
        "@hackage//:base",
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
    name = "libadd_one.so",
    srcs = [
        ":hs-lib-b.so",
    ],
    visibility = ["//tests:__subpackages__"],
    deps = ["@ghc//:threaded-rts"],
    linkstatic = 1,
    linkshared = 1,
)

# just dlopens libadd_one.so and prints it
py_binary(
    name = "python_add_one",
    srcs = ["python_add_one.py"],
    data = [
        ":libadd_one.so",
    ],
    visibility = ["//tests:__subpackages__"],
    deps = ["@bazel_tools//tools/python/runfiles"],
    srcs_version = "PY3ONLY",
    default_python_version = "PY3"
)
