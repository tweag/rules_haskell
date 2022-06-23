"""Workspace rules (tools/repositories)"""

load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_file")
load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl", "nixpkgs_package")
load("@contrib_rules_bazel_integration_test//bazel_integration_test:defs.bzl", "bazel_binaries")
load(
    "//:bazel_versions.bzl",
    "SUPPORTED_BAZEL_VERSIONS",
    "SUPPORTED_NIXPKGS_BAZEL_PACKAGES",
)

def rules_haskell_worker_dependencies(**stack_kwargs):
    """
    Provide all repositories that are necessary for `rules_haskell`'s tools to
    function.
    """
    excludes = native.existing_rules().keys()

    if "rules_haskell_worker_dependencies" not in excludes:
        stack_snapshot(
            name = "rules_haskell_worker_dependencies",
            packages = [
                "base",
                "bytestring",
                "filepath",
                "ghc",
                "ghc-paths",
                "microlens",
                "process",
                "profunctors-5.5.2",
                "proto-lens-0.7.0.0",
                "proto-lens-runtime-0.7.0.0",
                "text",
                "vector",
            ],
            snapshot = "lts-18.0",
            **stack_kwargs
        )

def bazel_binaries_for_integration_testing():
    bazel_binaries(versions = SUPPORTED_BAZEL_VERSIONS)
    for package in SUPPORTED_NIXPKGS_BAZEL_PACKAGES:
        nixpkgs_package(
            name = package,
            repository = "@nixpkgs_default",
            build_file_content = """\
filegroup(
    name = "bazel_bin",
    srcs = ["bin/bazel"],
    visibility = [ "//visibility:public" ],
)
""",
        )

def nixpkgs_bazel_label(package):
    return "@%s//:bazel_bin" % package
