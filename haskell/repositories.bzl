"""Workspace rules (repositories)"""

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

def rules_haskell_dependencies():
    """Provide all repositories that are necessary for `rules_haskell` to
    function.
    """
    excludes = native.existing_rules().keys()

    if "platforms" not in excludes:
        http_archive(
            name = "platforms",
            sha256 = "23566db029006fe23d8140d14514ada8c742d82b51973b4d331ee423c75a0bfa",
            strip_prefix = "platforms-46993efdd33b73649796c5fc5c9efb193ae19d51",
            urls = ["https://github.com/bazelbuild/platforms/archive/46993efdd33b73649796c5fc5c9efb193ae19d51.tar.gz"],
        )

    if "bazel_skylib" not in excludes:
        http_archive(
            name = "bazel_skylib",
            sha256 = "eb5c57e4c12e68c0c20bc774bfbc60a568e800d025557bc4ea022c6479acc867",
            strip_prefix = "bazel-skylib-0.6.0",
            urls = ["https://github.com/bazelbuild/bazel-skylib/archive/0.6.0.tar.gz"],
        )

    if "rules_cc" not in excludes:
        http_archive(
            name = "rules_cc",
            sha256 = "dafda2ff2a913028ce1718253b6b2f353b2d2163470f3069ca810a0d8d55a5a9",
            strip_prefix = "rules_cc-cd7e8a690caf526e0634e3ca55b10308ee23182d",
            urls = ["https://github.com/bazelbuild/rules_cc/archive/cd7e8a690caf526e0634e3ca55b10308ee23182d.tar.gz"],
        )

    if "rules_python" not in excludes:
        http_archive(
            name = "rules_python",
            sha256 = "fa53cc0afe276d8f6675df1a424592e00e4f37b2a497e48399123233902e2e76",
            strip_prefix = "rules_python-0.0.1",
            urls = ["https://github.com/bazelbuild/rules_python/archive/0.0.1.tar.gz"],
        )

    if "rules_sh" not in excludes:
        http_archive(
            name = "rules_sh",
            sha256 = "8f2722359c0e13a258c341aac69b8faa96b21e8f3382bd375d78c52f8b5a3d34",
            strip_prefix = "rules_sh-0.1.1",
            urls = ["https://github.com/tweag/rules_sh/archive/v0.1.1.tar.gz"],
        )

    if "io_tweag_rules_nixpkgs" not in excludes:
        http_archive(
            name = "io_tweag_rules_nixpkgs",
            sha256 = "3c5079748d27680501a81cc990333296847b52f6ad2ae9d49709f96f0137c7a3",
            strip_prefix = "rules_nixpkgs-7e7f5a5fb8817b51beea16837ce1e087dd304fd1",
            urls = ["https://github.com/tweag/rules_nixpkgs/archive/7e7f5a5fb8817b51beea16837ce1e087dd304fd1.tar.gz"],
        )

    if "com_google_protobuf" not in excludes:
        http_archive(
            name = "com_google_protobuf",
            sha256 = "e8c7601439dbd4489fe5069c33d374804990a56c2f710e00227ee5d8fd650e67",
            strip_prefix = "protobuf-3.11.2",
            urls = [
                "https://mirror.bazel.build/github.com/google/protobuf/archive/v3.11.2.tar.gz",
                "https://github.com/google/protobuf/archive/v3.11.2.tar.gz",
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
    if "zlib" not in excludes:
        http_archive(
            name = "zlib",
            build_file = "@com_google_protobuf//:third_party/zlib.BUILD",
            sha256 = "629380c90a77b964d896ed37163f5c3a34f6e6d897311f1df2a7016355c45eff",
            strip_prefix = "zlib-1.2.11",
            urls = ["https://github.com/madler/zlib/archive/v1.2.11.tar.gz"],
        )

def haskell_repositories():
    """DEPRECATED alias for rules_haskell_dependencies"""
    rules_haskell_dependencies()
