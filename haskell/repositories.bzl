"""Workspace rules (repositories)"""

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

def rules_haskell_dependencies():
    """Provide all repositories that are necessary for `rules_haskell` to function."""
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
            sha256 = "e5d90f0ec952883d56747b7604e2a15ee36e288bb556c3d0ed33e818a4d971f2",
            strip_prefix = "bazel-skylib-1.0.2",
            urls = ["https://github.com/bazelbuild/bazel-skylib/archive/1.0.2.tar.gz"],
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
            sha256 = "8371b7b7b05ef374719dbedd1fdf2c0225b158d997f6043e55a437e4dfb98a95",
            strip_prefix = "rules_sh-0c274ad480ed3eade49250abd04ff71655a07820",
            urls = ["https://github.com/tweag/rules_sh/archive/0c274ad480ed3eade49250abd04ff71655a07820.tar.gz"],
        )

    if "io_tweag_rules_nixpkgs" not in excludes:
        http_archive(
            name = "io_tweag_rules_nixpkgs",
            sha256 = "1bcc4a040b083b6ac436b49a8b4c22885274ee47c6fe8b353846a7f68bad852b",
            strip_prefix = "rules_nixpkgs-531804b50fc373de73984e697300beaf94e260c7",
            urls = ["https://github.com/tweag/rules_nixpkgs/archive/531804b50fc373de73984e697300beaf94e260c7.tar.gz"],
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
    """Alias for rules_haskell_dependencies

    Deprecated:
      Use rules_haskell_dependencies instead.
    """
    rules_haskell_dependencies()
