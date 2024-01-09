load("@os_info//:os_info.bzl", "is_nix_shell", "is_windows")
load(
    "@rules_bazel_integration_test//bazel_integration_test:defs.bzl",
    "integration_test_utils",
)
load(
    "@rules_haskell//haskell:private/versions.bzl",
    "SUPPORTED_BAZEL_VERSIONS",
    "SUPPORTED_NIXPKGS_BAZEL_PACKAGES",
)
load("//tests/integration_testing:dependencies.bzl", "nixpkgs_bazel_label")
load(":haskell_bazel_integration_test.bzl", "haskell_bazel_integration_test")

def rules_haskell_integration_test(
        name,
        workspace_path,
        srcs,
        deps = [],
        bindist_bazel_versions = SUPPORTED_BAZEL_VERSIONS,
        nixpkgs_bazel_packages = SUPPORTED_NIXPKGS_BAZEL_PACKAGES,
        **kwargs):
    bindist_bazel_binaries = {
        version.replace(".", "_"): integration_test_utils.bazel_binary_label(version)
        for version in bindist_bazel_versions
    }
    nixpkgs_bazel_binaries = {
        package.replace(".", "_"): nixpkgs_bazel_label(package)
        for package in nixpkgs_bazel_packages
    }

    if is_nix_shell:
        haskell_bazel_integration_test(
            name = "%s_nixpkgs" % name,
            srcs = srcs,
            deps = deps,
            env = {
                "NIXPKGS": "1",
            },
            bazel_binaries = nixpkgs_bazel_binaries,
            workspace_path = workspace_path,
            rule_files = ["@rules_haskell//:distribution"],
            target_compatible_with = select({
                "//tests:nix": [],
                "//conditions:default": ["@platforms//:incompatible"],
            }),
            **kwargs
        )
    elif not is_windows:
        haskell_bazel_integration_test(
            name = "%s_bindist" % name,
            srcs = srcs,
            deps = deps,
            bazel_binaries = bindist_bazel_binaries,
            workspace_path = workspace_path,
            rule_files = ["@rules_haskell//:distribution"],
            target_compatible_with = select({
                "//tests:nix": ["@platforms//:incompatible"],
                "//conditions:default": [],
            }),
            **kwargs
        )
