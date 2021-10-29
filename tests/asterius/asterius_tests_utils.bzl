load(
    "@rules_haskell//haskell/asterius:defs.bzl",
    "ahc_dist",
    "asterius_binary",
    "asterius_test",
    "asterius_webpack",
)

tags = ["dont_test_on_windows", "dont_test_on_darwin", "skip_profiling", "dont_test_on_bazel_lt_4"]

def asterius_test_macro(dep_label, suffix = ""):
    ahc_dist(
        name = "test_ahc_dist" + suffix,
        dep = dep_label,
        main_output = "custom_output_name.mjs",
        target = "node",
        testonly = True,
        tags = tags,
    )

    ahc_dist(
        name = "test_ahc_dist_browser" + suffix,
        dep = dep_label,
        main_output = "custom_output_name.mjs",
        target = "browser",
        testonly = True,
        tags = tags,
    )

    asterius_test(
        name = "asterius_test" + suffix,
        ahc_dist_dep = ":test_ahc_dist" + suffix,
        entry_point = "custom_output_name.mjs",
        testonly = True,
        tags = tags,
    )

    asterius_webpack(
        name = "asterius_bundle" + suffix,
        ahc_dist_dep = ":test_ahc_dist_browser" + suffix,
        testonly = True,
        tags = tags,
    )
