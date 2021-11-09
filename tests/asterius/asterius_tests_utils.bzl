load(
    "@rules_haskell//haskell/asterius:defs.bzl",
    "ahc_dist",
    "asterius_binary",
    "asterius_test",
    "asterius_webpack",
)

tags = ["dont_test_on_windows", "dont_test_on_darwin", "skip_profiling", "dont_test_on_bazel_lt_4"]

def asterius_test_macro(
        dep_label,
        suffix = "",
        test_subfolder_name = None,
        test_entry_point = None):
    ahc_dist(
        name = "test_ahc_dist" + suffix,
        dep = dep_label,
        entry_point = test_entry_point,
        subfolder_name = test_subfolder_name,
        testonly = True,
        tags = tags,
    )

    asterius_test(
        name = "asterius_test" + suffix,
        ahc_dist_dep = ":test_ahc_dist" + suffix,
        entry_point = test_entry_point,
        testonly = True,
        tags = tags,
        subfolder_name = test_subfolder_name,
    )

    asterius_binary(
        name = "asterius_binary" + suffix,
        ahc_dist_dep = ":test_ahc_dist" + suffix,
        entry_point = test_entry_point,
        testonly = True,
        tags = tags,
        subfolder_name = test_subfolder_name,
    )

    asterius_webpack(
        name = "asterius_bundle" + suffix,
        ahc_dist_dep = ":test_ahc_dist" + suffix,
        testonly = True,
        tags = tags,
    )

    native.genrule(
        name = "asterius_binary_from_genrule" + suffix,
        srcs = [],
        outs = ["out" + suffix],
        cmd = "./$(location :asterius_binary{}) > \"$@\"".format(suffix),
        exec_tools = [":asterius_binary" + suffix],
        testonly = True,
        tags = tags,
    )
