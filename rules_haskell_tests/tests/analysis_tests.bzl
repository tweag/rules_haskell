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

def _action_test_impl(ctx):
    env = analysistest.begin(ctx)

    target_under_test = analysistest.target_under_test(env)
    actions = [
        action
        for action in analysistest.target_actions(env)
        if action.mnemonic == ctx.attr.mnemonic
    ]
    asserts.true(env, len(actions) > 0, "Expected a {} action for {}.".format(ctx.attr.mnemonic, target_under_test.label))
    for action in actions:
        for key in ctx.attr.env_contains:
            asserts.true(
                env,
                key in action.env,
                "Expected {} in the environment of the {} action, got {}.".format(key, ctx.attr.mnemonic, action.env),
            )
        for key in ctx.attr.env_not_contains:
            asserts.false(
                env,
                key in action.env,
                "Expected no {} in the environment of the {} action, got {}.".format(key, ctx.attr.mnemonic, action.env),
            )
        for arg in ctx.attr.argv_contains:
            asserts.true(
                env,
                arg in action.argv,
                "Expected {} in the arguments of the {} action, got {}.".format(arg, ctx.attr.mnemonic, action.argv),
            )
        for arg in ctx.attr.argv_not_contains:
            asserts.false(
                env,
                arg in action.argv,
                "Expected no {} in the arguments of the {} action, got {}.".format(arg, ctx.attr.mnemonic, action.argv),
            )

    return analysistest.end(env)

action_test = analysistest.make(
    _action_test_impl,
    attrs = {
        "argv_contains": attr.string_list(
            doc = "Arguments that the action must have.",
        ),
        "argv_not_contains": attr.string_list(
            doc = "Arguments that the action must not have.",
        ),
        "env_contains": attr.string_list(
            doc = "Environment variables that the action must set.",
        ),
        "env_not_contains": attr.string_list(
            doc = "Environment variables that the action must not set.",
        ),
        "mnemonic": attr.string(
            doc = "Mnemonic of the actions to check.",
            mandatory = True,
        ),
    },
)
