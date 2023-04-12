"""Workspace rules (repositories)"""

load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@bazel_tools//tools/build_defs/repo:utils.bzl", "maybe")
load(
    ":private/versions.bzl",
    "check_bazel_version_compatible",
)

_rules_nixpkgs_version = "fb0e76e093c6ba7987ea4276027507a0d6dceb06"
_rules_nixpkgs_sha256 = "a8f20854da16156bd4e0d53d13dfb0d3360ee43c5ce764f0dc60703ab2b910e5"

_rules_sh_version = "v0.3.0"
_rules_sh_sha256 = "d668bb32f112ead69c58bde2cae62f6b8acefe759a8c95a2d80ff6a85af5ac5e"

def rules_haskell_dependencies():
    """Provide all repositories that are necessary for `rules_haskell` to function."""
    if "bazel_version" in dir(native):
        check_bazel_version_compatible(native.bazel_version)

    maybe(
        http_archive,
        name = "platforms",
        urls = [
            "https://mirror.bazel.build/github.com/bazelbuild/platforms/releases/download/0.0.6/platforms-0.0.6.tar.gz",
            "https://github.com/bazelbuild/platforms/releases/download/0.0.6/platforms-0.0.6.tar.gz",
        ],
        sha256 = "5308fc1d8865406a49427ba24a9ab53087f17f5266a7aabbfc28823f3916e1ca",
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
        sha256 = "48a838a6e1983e4884b26812b2c748a35ad284fd339eb8e2a6f3adf95307fbcd",
        strip_prefix = "rules_python-0.16.2",
        url = "https://github.com/bazelbuild/rules_python/archive/refs/tags/0.16.2.tar.gz",
    )

    maybe(
        http_archive,
        name = "rules_sh",
        urls = ["https://github.com/tweag/rules_sh/archive/%s.tar.gz" % _rules_sh_version],
        sha256 = _rules_sh_sha256,
        strip_prefix = "rules_sh-%s" % _rules_sh_version.lstrip("v"),
    )

    if "io_tweag_rules_nixpkgs" not in native.existing_rules():
        # N.B. rules_nixpkgs was split into separate components, which need to be loaded separately
        #
        # See https://github.com/tweag/rules_nixpkgs/issues/182 for the rational

        strip_prefix = "rules_nixpkgs-%s" % _rules_nixpkgs_version.lstrip("v")

        http_archive(
            name = "io_tweag_rules_nixpkgs",
            strip_prefix = strip_prefix,
            urls = ["https://github.com/tweag/rules_nixpkgs/archive/%s.tar.gz" % _rules_nixpkgs_version],
            sha256 = _rules_nixpkgs_sha256,
        )

        # requiered by rules_nixpkgs
        http_archive(
            name = "rules_nodejs",
            sha256 = "08337d4fffc78f7fe648a93be12ea2fc4e8eb9795a4e6aa48595b66b34555626",
            urls = ["https://github.com/bazelbuild/rules_nodejs/releases/download/5.8.0/rules_nodejs-core-5.8.0.tar.gz"],
        )

        http_archive(
            name = "rules_nixpkgs_core",
            strip_prefix = strip_prefix + "/core",
            urls = ["https://github.com/tweag/rules_nixpkgs/archive/%s.tar.gz" % _rules_nixpkgs_version],
            sha256 = _rules_nixpkgs_sha256,
        )

        for toolchain in ["cc", "java", "python", "go", "rust", "posix", "nodejs"]:
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
        name = "aspect_rules_js",
        sha256 = "2a1e5d4400e2b49f6d36785aa894412670a0babfe7054e733b6a8f23c1b41e26",
        strip_prefix = "rules_js-1.23.1",
        url = "https://github.com/aspect-build/rules_js/releases/download/v1.23.1/rules_js-v1.23.1.tar.gz",
    )

def haskell_repositories():
    """Alias for rules_haskell_dependencies

    Deprecated:
      Use rules_haskell_dependencies instead.
    """
    rules_haskell_dependencies()
