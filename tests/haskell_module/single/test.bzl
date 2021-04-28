load("@bazel_skylib//lib:paths.bzl", "paths")
load("@bazel_skylib//lib:unittest.bzl", "analysistest", "asserts")

def _default_info_test_impl(ctx):
    env = analysistest.begin(ctx)
    target_under_test = analysistest.target_under_test(env)

    default_info = target_under_test[DefaultInfo]

    obj_files = [
        f
        for f in default_info.files.to_list()
        if paths.split_extension(f.path)[1] == ".o"
    ]
    asserts.true(env, len(obj_files) == 1, "Expected exactly one object file")
    [obj_file] = obj_files
    asserts.equals(env, "Single.o", obj_file.basename)

    hi_files = [
        f
        for f in default_info.files.to_list()
        if paths.split_extension(f.path)[1] == ".hi"
    ]
    asserts.true(env, len(hi_files) == 1, "Expected exactly one interface file")
    [hi_file] = hi_files
    asserts.equals(env, "Single.hi", hi_file.basename)

    return analysistest.end(env)

default_info_test = analysistest.make(_default_info_test_impl)
