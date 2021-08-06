load("@bazel_skylib//lib:paths.bzl", "paths")
load("@bazel_skylib//lib:sets.bzl", "sets")
load("@bazel_skylib//lib:unittest.bzl", "analysistest", "asserts")

def _default_info_test_impl(ctx):
    env = analysistest.begin(ctx)
    target_under_test = analysistest.target_under_test(env)

    default_info = target_under_test[DefaultInfo]
    actual_files = [x.basename for x in default_info.files.to_list()]

    # can't use is_profiling_enabled(haskell_context(ctx)) here, because we
    # don't have access to the haskell toolchain
    with_profiling = ctx.var["COMPILATION_MODE"] == "dbg"

    if with_profiling:
        expected_files = ["Single.p_o", "Single.p_hi"]
    else:
        expected_files = ["Single.o", "Single.hi"]

    asserts.set_equals(env, sets.make(expected_files), sets.make(actual_files))

    return analysistest.end(env)

default_info_test = analysistest.make(_default_info_test_impl)
