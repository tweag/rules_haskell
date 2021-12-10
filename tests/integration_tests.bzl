load("@io_bazel_rules_go//go/tools/bazel_testing:def.bzl", "go_bazel_test")

def integration_test(name, **kwargs):
    test_src = name + ".go"
    size = kwargs.pop("size", "medium")

    native.config_setting(
        name = "nixpkgs",
        values = {"define": "nixpkgs=true"},
    )

    go_bazel_test(
        name = name,
        srcs = [test_src],
        size = size,
        rule_files = ["//:distribution"],
        args = select({
            ":nixpkgs": ["nixpkgs=true"],
            "//conditions:default": ["nixpkgs=false"],
        }),
        **kwargs
    )
