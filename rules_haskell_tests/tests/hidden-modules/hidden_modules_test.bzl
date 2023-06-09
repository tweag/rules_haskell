load("@bazel_skylib//lib:unittest.bzl", "analysistest", "asserts")
load("@rules_haskell//haskell:defs.bzl", "haskell_library")
load("@rules_haskell//haskell/experimental:defs.bzl", "haskell_module")

# Test for failure in the case of hiding non existent modules
def _hidden_module_test_impl(ctx):
    env = analysistest.begin(ctx)

    asserts.expect_failure(env, "Hidden modules must be a subset of all modules")

    return analysistest.end(env)

hidden_module_test = analysistest.make(
    _hidden_module_test_impl,
    expect_failure = True,
)

# test using the standard haskell library rules
def _test_hidden_modules1():
    haskell_library(
        name = "lib-hidden-1",
        srcs = native.glob(["lib-a/*.hs"]),
        src_strip_prefix = "lib-a",
        hidden_modules = ["NotHere"],
        deps = ["//tests/hackage:base"],
        tags = ["manual"],
    )

    hidden_module_test(
        name = "hidden_module_test-1",
        target_under_test = ":lib-hidden-1",
    )

# Test using haskell modules
def _test_hidden_modules2():
    haskell_module(
        name = "FooModule",
        src = "lib-a/Foo.hs",
        module_name = "Foo",
    )

    haskell_module(
        name = "BarModule",
        src = "lib-a/Bar.hs",
        module_name = "Bar",
    )

    haskell_library(
        name = "lib-hidden-2",
        modules = [
            ":FooModule",
            ":BarModule",
        ],
        # Should be [Bar] if you actually want to hide the Bar module
        hidden_modules = ["BarModule"],
        visibility = ["//visibility:public"],
        deps = [
            "//tests/hackage:base",
        ],
        tags = ["manual"],
    )

    hidden_module_test(
        name = "hidden_module_test-2",
        target_under_test = ":lib-hidden-2",
    )

# Run all of the expect failure tests
def hidden_modules_test_suite(name):
    _test_hidden_modules1()
    _test_hidden_modules2()

    native.test_suite(
        name = name,
        tests = [
            ":hidden_module_test-1",
            ":hidden_module_test-2",
        ],
    )
