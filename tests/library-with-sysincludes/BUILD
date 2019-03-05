load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
)

package(default_testonly = 1)

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
cc_library(
    name = "zlib",
    hdrs = ["@zlib.dev//:include"],
    strip_include_prefix = "/external/zlib.dev/include",
    tags = ["requires_zlib"],
    deps = ["@zlib"],
)

cc_library(
    name = "zlib-with-genrule-header",
    hdrs = [":genrule-header"],
    strip_include_prefix = "include",
    tags = ["requires_zlib"],
)

haskell_library(
    name = "intermediate-library",
    srcs = ["IntLib.hsc"],
    tags = ["requires_zlib"],
    deps = [
        ":zlib",
        ":zlib-with-genrule-header",
        "//tests/hackage:base",
    ],
)

haskell_library(
    name = "library-with-sysincludes",
    srcs = [
        "Lib.hs",
        "TH.hs",
    ],
    tags = ["requires_zlib"],
    visibility = ["//visibility:public"],
    deps = [
        ":intermediate-library",
        "//tests/hackage:base",
        "//tests/hackage:template-haskell",
    ],
)

# Replicate the above example, but use the externally-defined
# cc_library rule.
haskell_library(
    name = "intermediate-library-other",
    srcs = ["IntLib.hsc"],
    tags = ["requires_zlib"],
    deps = [
        ":zlib-with-genrule-header",
        "//tests/hackage:base",
        "@zlib.dev//:zlib",
    ],
)

haskell_library(
    name = "library-with-sysincludes-other",
    srcs = [
        "Lib.hs",
        "TH.hs",
    ],
    tags = ["requires_zlib"],
    visibility = ["//visibility:public"],
    deps = [
        ":intermediate-library-other",
        "//tests/hackage:base",
        "//tests/hackage:template-haskell",
    ],
)
