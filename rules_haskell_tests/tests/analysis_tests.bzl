load("@bazel_skylib//lib:sets.bzl", "sets")
load("@bazel_skylib//lib:unittest.bzl", "analysistest", "asserts")

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
