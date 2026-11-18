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

def _cabal_wrapper_extra_args_test_impl(ctx):
    env = analysistest.begin(ctx)

    target_under_test = analysistest.target_under_test(env)
    args_files = [
        action.content
        for action in analysistest.target_actions(env)
        if action.mnemonic == "FileWrite" and action.outputs.to_list()[0].basename.endswith("_cabal_wrapper_args.json")
    ]
    asserts.equals(env, 1, len(args_files), "Expected one cabal_wrapper arguments file for {}.".format(target_under_test.label))
    if len(args_files) == 1:
        extra_args = json.decode(args_files[0])["extra_args"]
        for arg in ctx.attr.contains:
            asserts.true(
                env,
                arg in extra_args,
                "Expected {} in the Cabal arguments of {}, got {}.".format(arg, target_under_test.label, extra_args),
            )
        for arg in ctx.attr.not_contains:
            asserts.false(
                env,
                arg in extra_args,
                "Expected no {} in the Cabal arguments of {}, got {}.".format(arg, target_under_test.label, extra_args),
            )

    return analysistest.end(env)

cabal_wrapper_extra_args_test = analysistest.make(
    _cabal_wrapper_extra_args_test_impl,
    attrs = {
        "contains": attr.string_list(
            doc = "Arguments that the cabal_wrapper extra_args must contain.",
        ),
        "not_contains": attr.string_list(
            doc = "Arguments that the cabal_wrapper extra_args must not contain.",
        ),
    },
)
