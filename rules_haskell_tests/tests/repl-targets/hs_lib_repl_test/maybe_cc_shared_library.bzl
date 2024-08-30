load("@bazel_features//:features.bzl", "bazel_features")

def maybe_cc_shared_library(name, **kwargs):
    if _has_cc_shared_library():
        shared_name = "%s_shared" % name
        native.cc_shared_library(
            name = shared_name,
            deps = [name],
            **kwargs,
        )
        return shared_name
    return name



def _has_cc_shared_library():
    return bazel_features.globals.CcSharedLibraryInfo != None
