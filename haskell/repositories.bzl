"""Workspace rules (repositories)"""

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@bazel_tools//tools/build_defs/repo:utils.bzl", "maybe")
load(":private/versions.bzl", "check_version")

def rules_haskell_dependencies():
    """Provide all repositories that are necessary for `rules_haskell` to function."""
    if "bazel_version" in dir(native):
        check_version(native.bazel_version)

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
            "https://github.com/bazelbuild/bazel-skylib/releases/download/1.0.3/bazel-skylib-1.0.3.tar.gz",
            "https://mirror.bazel.build/github.com/bazelbuild/bazel-skylib/releases/download/1.0.3/bazel-skylib-1.0.3.tar.gz",
        ],
        sha256 = "1c531376ac7e5a180e0237938a2536de0c54d93f5c278634818e0efc952dd56c",
    )

    maybe(
        http_archive,
        name = "rules_cc",
        sha256 = "34b2ebd4f4289ebbc27c7a0d854dcd510160109bb0194c0ba331c9656ffcb556",
        strip_prefix = "rules_cc-daf6ace7cfeacd6a83e9ff2ed659f416537b6c74",
        urls = ["https://github.com/bazelbuild/rules_cc/archive/daf6ace7cfeacd6a83e9ff2ed659f416537b6c74.tar.gz"],
    )

    maybe(
        http_archive,
        name = "rules_python",
        url = "https://github.com/bazelbuild/rules_python/releases/download/0.3.0/rules_python-0.3.0.tar.gz",
        sha256 = "934c9ceb552e84577b0faf1e5a2f0450314985b4d8712b2b70717dc679fdc01b",
    )

    maybe(
        http_archive,
        name = "rules_sh",
        sha256 = "83a065ba6469135a35786eb741e17d50f360ca92ab2897857475ab17c0d29931",
        strip_prefix = "rules_sh-0.2.0",
        urls = ["https://github.com/tweag/rules_sh/archive/v0.2.0.tar.gz"],
    )

    maybe(
        http_archive,
        name = "io_tweag_rules_nixpkgs",
        sha256 = "6bedf80d6cb82d3f1876e27f2ff9a2cc814d65f924deba14b49698bb1fb2a7f7",
        strip_prefix = "rules_nixpkgs-a388ab60dea07c3fc182453e89ff1a67c9d3eba6",
        urls = ["https://github.com/tweag/rules_nixpkgs/archive/a388ab60dea07c3fc182453e89ff1a67c9d3eba6.tar.gz"],
    )

    maybe(
        http_archive,
        name = "com_google_protobuf",
        sha256 = "f18a40816260a9a3190a94efb0fc26270b244a2436681602f0a944739095d632",
        strip_prefix = "protobuf-3.15.1",
        urls = [
            "https://github.com/protocolbuffers/protobuf/archive/refs/tags/v3.15.1.tar.gz",
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

def haskell_repositories():
    """Alias for rules_haskell_dependencies

    Deprecated:
      Use rules_haskell_dependencies instead.
    """
    rules_haskell_dependencies()
