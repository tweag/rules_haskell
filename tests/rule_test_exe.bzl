load("@bazel_tools//tools/build_rules:test_rules.bzl", "rule_test")

def rule_test_exe(generates, **kwargs):
    """
        Like 'rule_test' but appends ".exe" to the elements of
        "generates".
    """

    new_generates = select({
        "@bazel_tools//src/conditions:windows": [e + ".exe" for e in generates],
        "//conditions:default": generates,
    })

    rule_test(generates = new_generates, **kwargs)
