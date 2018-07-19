package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_binary",
    "haskell_cc_import",
    "haskell_library",
)

genrule(
    name = "genrule-header",
    outs = [
        "include/foo.h",
    ],
    cmd = "touch $(location include/foo.h)",
)

# A locally-defined replica of @zlib.dev//:zlib.
# Since that shared library lives in another package, we must
# use an absolute path for strip_include_prefix.
haskell_cc_import(
    name = "zlib",
    hdrs = ["@zlib.dev//:include"],
    shared_library = "@zlib//:lib",
    strip_include_prefix = "/external/zlib.dev/include",
)

haskell_cc_import(
    name = "zlib-with-genrule-header",
    hdrs = [":genrule-header"],
    shared_library = "@zlib//:lib",  # just use zlib because this field is required
    strip_include_prefix = "include",
)

haskell_library(
    name = "intermediate-library",
    srcs = ["IntLib.hsc"],
    prebuilt_dependencies = ["base"],
    deps = [
        ":zlib",
        ":zlib-with-genrule-header",
    ],
)

haskell_library(
    name = "library-with-sysincludes",
    srcs = [
        "Lib.hs",
        "TH.hs",
    ],
    prebuilt_dependencies = [
        "base",
        "template-haskell",
    ],
    visibility = ["//visibility:public"],
    deps = [":intermediate-library"],
)

# Replicate the above example, but use the externally-defined
# haskell_cc_import rule.
haskell_library(
    name = "intermediate-library-other",
    srcs = ["IntLib.hsc"],
    prebuilt_dependencies = ["base"],
    deps = [
        ":zlib-with-genrule-header",
        "@zlib.dev//:zlib",
    ],
)

haskell_library(
    name = "library-with-sysincludes-other",
    srcs = [
        "Lib.hs",
        "TH.hs",
    ],
    prebuilt_dependencies = [
        "base",
        "template-haskell",
    ],
    visibility = ["//visibility:public"],
    deps = [":intermediate-library-other"],
)
