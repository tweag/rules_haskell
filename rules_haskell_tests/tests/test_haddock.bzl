""" Compute expected results for the //tests:test-haddock test"""

load("@bazel_skylib//lib:new_sets.bzl", "sets")
load("@bazel_skylib//lib:unittest.bzl", "analysistest", "asserts")
load("@bazel_tools//tools/build_rules:test_rules.bzl", "strip_prefix")

def _is_version(token):
    if "." not in token:
        return False
    parts = token.split(".")
    for p in parts:
        if not p.isdigit():
            return False
    return True

def _strip_version_suffix(s):
    parts = s.split("-")
    if len(parts) < 2:
        return s
    if _is_version(parts[-1]):
        return "-".join(parts[:-1])
    if len(parts) < 3:
        return s
    if parts[-1].isalnum() and _is_version(parts[-2]):
        return "-".join(parts[:-2])
    return s

def _non_versioned_test_impl(ctx):
    env = analysistest.begin(ctx)

    target = analysistest.target_under_test(env)

    # Generate the proper prefix to remove from generated files.
    prefix_parts = []

    if target.label.workspace_root:
        # Create a prefix that is correctly relative to the output of this rule.
        prefix_parts = ["..", strip_prefix("external/", target.label.workspace_root)]

    if target.label.package:
        prefix_parts.append(target.label.package)

    if prefix_parts:
        prefix = "/".join(prefix_parts) + "/"
    else:
        prefix = ""

    generated = [
        _strip_version_suffix(strip_prefix(prefix, f.short_path))
        for f in target.files.to_list()
    ]
    asserts.new_set_equals(
        env,
        sets.make(ctx.attr.generates),
        sets.make(generated),
        "Generates expected files (sans version suffix)",
    )

    return analysistest.end(env)

non_versioned_output_test = analysistest.make(
    _non_versioned_test_impl,
    attrs = {
        "generates": attr.string_list(),
    },
)
