load("@bazel_skylib//lib:unittest.bzl", "analysistest", "asserts")
load("@rules_haskell//haskell:defs.bzl","haskell_library")

def _hidden_module_test_impl(ctx):
    env = analysistest.begin(ctx)

    asserts.expect_failure(env, "Hidden modules must be a subset of all modules")

    return analysistest.end(env)

hidden_module_test = analysistest.make(
    _hidden_module_test_impl,
    expect_failure = True,
)

def _test_hidden_modules():

    haskell_library(
        name = "lib-d",
        srcs = native.glob(["lib-a/*.hs"]),
        src_strip_prefix = "lib-a",
        hidden_modules = ["NotHere"],
        deps = ["//tests/hackage:base"],
        tags = ["manual"],
    )

    hidden_module_test(
        name = "hidden_module_test",
        target_under_test = ":lib-d",
    )

def hidden_modules_test_suite(name):
    _test_hidden_modules()

    native.test_suite(
        name = name,
        tests = [
            ":hidden_module_test",
        ],
    )