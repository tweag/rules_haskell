load("@io_bazel_rules_go//go/tools/bazel_testing:def.bzl", "go_bazel_test")

def integration_test(name, **kwargs):
    test_src = name + ".go"
    size = kwargs.pop("size", "medium")
    kwargs.setdefault("deps", [])

    it_library = "@rules_haskell//tests:integration_testing"
    if it_library not in kwargs["deps"]:
        kwargs["deps"] += [it_library]

    go_bazel_test(
        name = name,
        srcs = [test_src],
        size = size,
        rule_files = ["//:distribution"],
        args = select({
            "//tests:nix": ["nixpkgs=true"],
            "//conditions:default": ["nixpkgs=false"],
        }) + select({
            "@platforms//os:osx": ["bazel_bin=$(location @bazel_bin_darwin//file)"],
            "@platforms//os:linux": ["bazel_bin=$(location @bazel_bin_linux//file)"],
            "@platforms//os:windows": ["bazel_bin=$(location @bazel_bin_windows//file)"],
        }),
        data = select({
            "@platforms//os:osx": ["@bazel_bin_darwin//file"],
            "@platforms//os:linux": ["@bazel_bin_linux//file"],
            "@platforms//os:windows": ["@bazel_bin_windows//file"],
        }),
        **kwargs
    )
