"""Workspace rules (tools/repositories)"""

load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_file")
load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl", "nixpkgs_package")

def rules_haskell_worker_dependencies(**stack_kwargs):
    """Provide all repositories that are necessary for `rules_haskell`'s tools to
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
                "mtl",
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
    http_file(
        name = "bazel_bin_linux",
        executable = True,
        sha256 = "0eb2e378d2782e7810753e2162245ad1179c1bb12f848c692b4a595b4edf779b",
        urls = ["https://github.com/bazelbuild/bazel/releases/download/4.1.0/bazel-4.1.0-linux-x86_64"],
    )

    http_file(
        name = "bazel_bin_darwin",
        executable = True,
        sha256 = "2eecc3abb0ff653ed0bffdb9fbfda7b08548c2868f13da4a995f01528db200a9",
        urls = ["https://github.com/bazelbuild/bazel/releases/download/4.1.0/bazel-4.1.0-darwin-x86_64"],
    )

    http_file(
        name = "bazel_bin_windows",
        executable = True,
        sha256 = "7b2077af7055b421fe31822f83c3c3c15e36ff39b69560ba2472dde92dd45b46",
        urls = ["https://github.com/bazelbuild/bazel/releases/download/4.1.0/bazel-4.1.0-windows-x86_64.exe"],
    )

    nixpkgs_package(
        name = "bazel_4",
        repository = "@nixpkgs_default",
        build_file_content = """\
filegroup(
    name = "bazel_bin",
    srcs = ["bin/bazel"],
    visibility = [ "//visibility:public" ],
)
""",
    )
