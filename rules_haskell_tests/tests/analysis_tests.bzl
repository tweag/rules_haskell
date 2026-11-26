load("@bazel_skylib//lib:sets.bzl", "sets")
load("@bazel_skylib//lib:unittest.bzl", "analysistest", "asserts")
load("@rules_cc//cc:defs.bzl", "CcInfo")

def _dynamic_libraries_in_runfiles_test_impl(ctx):
    env = analysistest.begin(ctx)

    target_under_test = analysistest.target_under_test(env)
    runfiles = sets.make(target_under_test[DefaultInfo].default_runfiles.files.to_list())

    dynamic_libs = [
        lib_to_link.dynamic_library
        for target in ctx.attr.libs
        for input in target[CcInfo].linking_context.linker_inputs.to_list()
        for lib_to_link in input.libraries
        if lib_to_link.dynamic_library != None
    ]
    for lib in dynamic_libs:
        asserts.true(
            env,
            sets.contains(runfiles, lib),
            "Expected library {} to be contained in the runfiles, but it was missing.".format(lib.short_path),
        )

    return analysistest.end(env)

dynamic_libraries_in_runfiles_test = analysistest.make(
    _dynamic_libraries_in_runfiles_test_impl,
    attrs = {
        "libs": attr.label_list(
            doc = "Check for these dynamic libraries among the runfiles.",
            providers = [CcInfo],
        ),
    },
)

def _analysis_failure_test_impl(ctx):
    env = analysistest.begin(ctx)
    asserts.expect_failure(env, ctx.attr.expected_error)
    return analysistest.end(env)

analysis_failure_test = analysistest.make(
    _analysis_failure_test_impl,
    doc = """Checks that the analysis of `target_under_test` fails with `expected_error`.

    Tag `target_under_test` as `manual`, so that `bazel build //...` does not try to build it.""",
    attrs = {
        "expected_error": attr.string(
            doc = "A substring of the expected error message.",
            mandatory = True,
        ),
    },
    expect_failure = True,
)
