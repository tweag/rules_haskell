load("@bazel_skylib//lib:selects.bzl", "selects")
load(
    "@rules_haskell//haskell/asterius:defs.bzl",
    "ahc_dist",
    "asterius_binary",
    "asterius_test",
    "asterius_webpack",
)

tags = ["dont_test_on_windows", "skip_profiling", "dont_test_on_bazel_lt_4"]

target_compatible_with = selects.with_or({
    ("@platforms//os:macos"): ["@platforms//:incompatible"],
    "//conditions:default": [],
})

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
        target_compatible_with = target_compatible_with,
        exec_compatible_with = target_compatible_with,
    )

    asterius_test(
        name = "asterius_test" + suffix,
        ahc_dist_dep = ":test_ahc_dist" + suffix,
        entry_point = entry_point,
        testonly = True,
        tags = tags,
        target_compatible_with = target_compatible_with,
        exec_compatible_with = target_compatible_with,
    )

    asterius_binary(
        name = "asterius_binary" + suffix,
        ahc_dist_dep = ":test_ahc_dist" + suffix,
        entry_point = entry_point,
        testonly = True,
        tags = tags,
        target_compatible_with = target_compatible_with,
        exec_compatible_with = target_compatible_with,
    )

    # TODO the following `asterius_binary_from_genrule` rule is meant to test
    # running a WebAssembly based js_binary as a build action.
    # It is set to `manual` and left for future work as rules_haskell WebAssembly handling may
    # need significant refactoring in the move from asterius to ghc's WebAssembly backend.
    native.genrule(
        name = "asterius_binary_from_genrule" + suffix,
        outs = ["out" + suffix],
        cmd = "BAZEL_BINDIR=$(BINDIR) $(location :asterius_binary{}) > \"$@\"".format(suffix),
        tools = [":asterius_binary" + suffix],
        testonly = True,
        tags = tags + ["manual"],
        target_compatible_with = target_compatible_with,
        exec_compatible_with = target_compatible_with,
    )

    asterius_webpack(
        name = "bundle" + suffix,
        ahc_dist_dep = "test_ahc_dist" + suffix,
        entry_point = entry_point,
        testonly = True,
        tags = tags,
        target_compatible_with = target_compatible_with,
        exec_compatible_with = target_compatible_with,
    )
