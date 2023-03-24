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
    subfolder_name = test_subfolder_name or "asterius_test_ahc_dist_{}".format(suffix)
    entry_point = "{}/{}".format(subfolder_name, test_entry_point or "test_ahc_dist.mjs")

    ahc_dist(
        name = "test_ahc_dist" + suffix,
        dep = dep_label,
        entry_point = entry_point,
        testonly = True,
        tags = tags,
    )

    asterius_test(
        name = "asterius_test" + suffix,
        ahc_dist_dep = ":test_ahc_dist" + suffix,
        entry_point = entry_point,
        testonly = True,
        tags = tags,
    )

    asterius_binary(
        name = "asterius_binary" + suffix,
        ahc_dist_dep = ":test_ahc_dist" + suffix,
        entry_point = entry_point,
        testonly = True,
        tags = tags,
    )

    asterius_webpack(
        name = "bundle" + suffix,
        ahc_dist_dep = "test_ahc_dist" + suffix,
        entry_point = entry_point,
        testonly = True,
        tags = tags,
    )
