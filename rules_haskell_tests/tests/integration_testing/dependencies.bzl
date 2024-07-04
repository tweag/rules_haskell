load("@rules_bazel_integration_test//bazel_integration_test:defs.bzl", "bazel_binaries")
load(
    "@rules_haskell//haskell:private/versions.bzl",
    "SUPPORTED_BAZEL_VERSIONS",
    "SUPPORTED_NIXPKGS_BAZEL_PACKAGES",
)
load("@rules_nixpkgs_core//:nixpkgs.bzl", "nixpkgs_package")

def integration_testing_bazel_binaries():
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
            fail_not_supported = False,
        )

def nixpkgs_bazel_label(package):
    return "@%s//:bazel_bin" % package
