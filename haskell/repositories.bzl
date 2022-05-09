"""Workspace rules (repositories)"""

load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@bazel_tools//tools/build_defs/repo:utils.bzl", "maybe")
load(
    ":private/versions.bzl",
    "check_bazel_version_compatible",
)

_rules_nixpkgs_version = "210d30a81cedde04b4281fd163428722278fddfb"
_rules_nixpkgs_sha256 = "61b24e273821a15146f9ae7577e64b53f6aa332d5a7056abe8221ae2c346fdbd"

def rules_haskell_dependencies():
    """Provide all repositories that are necessary for `rules_haskell` to function."""
    if "bazel_version" in dir(native):
        check_bazel_version_compatible(native.bazel_version)

    maybe(
        http_archive,
        name = "platforms",
        urls = [
            "https://mirror.bazel.build/github.com/bazelbuild/platforms/releases/download/0.0.4/platforms-0.0.4.tar.gz",
            "https://github.com/bazelbuild/platforms/releases/download/0.0.4/platforms-0.0.4.tar.gz",
        ],
        sha256 = "079945598e4b6cc075846f7fd6a9d0857c33a7afc0de868c2ccb96405225135d",
    )

    maybe(
        http_archive,
        name = "bazel_skylib",
        urls = [
            "https://github.com/bazelbuild/bazel-skylib/releases/download/1.1.1/bazel-skylib-1.1.1.tar.gz",
            "https://mirror.bazel.build/github.com/bazelbuild/bazel-skylib/releases/download/1.1.1/bazel-skylib-1.1.1.tar.gz",
        ],
        sha256 = "c6966ec828da198c5d9adbaa94c05e3a1c7f21bd012a0b29ba8ddbccb2c93b0d",
    )

    maybe(
        http_archive,
        name = "rules_cc",
        urls = ["https://github.com/bazelbuild/rules_cc/releases/download/0.0.1/rules_cc-0.0.1.tar.gz"],
        sha256 = "4dccbfd22c0def164c8f47458bd50e0c7148f3d92002cdb459c2a96a68498241",
    )

    maybe(
        http_archive,
        name = "rules_python",
        url = "https://github.com/bazelbuild/rules_python/releases/download/0.5.0/rules_python-0.5.0.tar.gz",
        sha256 = "cd6730ed53a002c56ce4e2f396ba3b3be262fd7cb68339f0377a45e8227fe332",
    )

    maybe(
        git_repository,
        name = "rules_sh",
        commit = "670efdc93c97fde478a91abb7d2b8308cb835a40",
        remote = "https://github.com/tweag/rules_sh.git",
        shallow_since = "1637931407 +0100",
    )

    if "io_tweag_rules_nixpkgs" not in native.existing_rules():
        # N.B. rules_nixpkgs was split into separate components, which need to be loaded separately
        #
        # See https://github.com/tweag/rules_nixpkgs/issues/182 for the rational

        strip_prefix = "rules_nixpkgs-%s" % _rules_nixpkgs_version

        http_archive(
            name = "io_tweag_rules_nixpkgs",
            strip_prefix = strip_prefix,
            urls = ["https://github.com/tweag/rules_nixpkgs/archive/%s.tar.gz" % _rules_nixpkgs_version],
            sha256 = _rules_nixpkgs_sha256,
        )

        http_archive(
            name = "rules_nixpkgs_core",
            strip_prefix = strip_prefix + "/core",
            urls = ["https://github.com/tweag/rules_nixpkgs/archive/%s.tar.gz" % _rules_nixpkgs_version],
            sha256 = _rules_nixpkgs_sha256,
        )

        for toolchain in ["cc", "java", "python", "go", "rust", "posix"]:
            http_archive(
                name = "rules_nixpkgs_" + toolchain,
                strip_prefix = strip_prefix + "/toolchains/" + toolchain,
                urls = ["https://github.com/tweag/rules_nixpkgs/archive/%s.tar.gz" % _rules_nixpkgs_version],
                sha256 = _rules_nixpkgs_sha256,
            )

    maybe(
        http_archive,
        name = "com_google_protobuf",
        sha256 = "87407cd28e7a9c95d9f61a098a53cf031109d451a7763e7dd1253abf8b4df422",
        strip_prefix = "protobuf-3.19.1",
        urls = [
            "https://github.com/protocolbuffers/protobuf/archive/refs/tags/v3.19.1.tar.gz",
        ],
    )

    # Dependency of com_google_protobuf.
    # TODO(judahjacobson): this is a bit of a hack.
    # We can't call that repository's protobuf_deps() function
    # from here, because load()ing it from this .bzl file would lead
    # to a cycle:
    # https://github.com/bazelbuild/bazel/issues/1550
    # https://github.com/bazelbuild/bazel/issues/1943
    # For now, just hard-code the subset that's needed to use `protoc`.
    # Alternately, consider adding another function from another
    # .bzl file that needs to be called from WORKSPACE, similar to:
    # https://github.com/grpc/grpc/blob/8c9dcf7c35e489c2072a9ad86635dbc4e28f88ea/bazel/grpc_extra_deps.bzl#L10
    maybe(
        http_archive,
        name = "zlib",
        build_file = "@com_google_protobuf//:third_party/zlib.BUILD",
        sha256 = "629380c90a77b964d896ed37163f5c3a34f6e6d897311f1df2a7016355c45eff",
        strip_prefix = "zlib-1.2.11",
        urls = ["https://github.com/madler/zlib/archive/v1.2.11.tar.gz"],
    )

    maybe(
        http_archive,
        name = "build_bazel_rules_nodejs",
        sha256 = "cfc289523cf1594598215901154a6c2515e8bf3671fd708264a6f6aefe02bf39",
        urls = ["https://github.com/bazelbuild/rules_nodejs/releases/download/4.4.6/rules_nodejs-4.4.6.tar.gz"],
    )

def haskell_repositories():
    """Alias for rules_haskell_dependencies

    Deprecated:
      Use rules_haskell_dependencies instead.
    """
    rules_haskell_dependencies()
