"""Workspace rules (repositories)"""

load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@bazel_tools//tools/build_defs/repo:utils.bzl", "maybe")
load(
    ":private/versions.bzl",
    "check_bazel_version_compatible",
)
load(":private/ghc_ci.bzl", "ghc_default_version")

_rules_nixpkgs_version = "0.10.0"
_rules_nixpkgs_sha256 = "980edfceef2e59e1122d9be6c52413bc298435f0a3d452532b8a48d7562ffd67"

_rules_sh_version = "v0.3.0"
_rules_sh_sha256 = "d668bb32f112ead69c58bde2cae62f6b8acefe759a8c95a2d80ff6a85af5ac5e"

def rules_haskell_dependencies_bzlmod():
    """Provide rules_haskell dependencies which are not available as bzlmod modules."""

    maybe(
        ghc_default_version,
        name = "rules_haskell_ghc_version",
    )

def rules_haskell_dependencies():
    """Provide all repositories that are necessary for `rules_haskell` to function."""
    if "bazel_version" in dir(native):
        check_bazel_version_compatible(native.bazel_version)

    maybe(
        http_archive,
        name = "platforms",
        urls = [
            "https://mirror.bazel.build/github.com/bazelbuild/platforms/releases/download/0.0.8/platforms-0.0.8.tar.gz",
            "https://github.com/bazelbuild/platforms/releases/download/0.0.8/platforms-0.0.8.tar.gz",
        ],
        sha256 = "8150406605389ececb6da07cbcb509d5637a3ab9a24bc69b1101531367d89d74",
    )

    maybe(
        http_archive,
        name = "bazel_skylib",
        urls = [
            "https://mirror.bazel.build/github.com/bazelbuild/bazel-skylib/releases/download/1.5.0/bazel-skylib-1.5.0.tar.gz",
            "https://github.com/bazelbuild/bazel-skylib/releases/download/1.5.0/bazel-skylib-1.5.0.tar.gz",
        ],
        sha256 = "cd55a062e763b9349921f0f5db8c3933288dc8ba4f76dd9416aac68acee3cb94",
    )

    maybe(
        http_archive,
        name = "rules_cc",
        urls = ["https://github.com/bazelbuild/rules_cc/releases/download/0.0.9/rules_cc-0.0.9.tar.gz"],
        sha256 = "2037875b9a4456dce4a79d112a8ae885bbc4aad968e6587dca6e64f3a0900cdf",
        strip_prefix = "rules_cc-0.0.9",
    )

    maybe(
        http_archive,
        name = "rules_python",
        sha256 = "94750828b18044533e98a129003b6a68001204038dc4749f40b195b24c38f49f",
        strip_prefix = "rules_python-0.21.0",
        url = "https://github.com/bazelbuild/rules_python/releases/download/0.21.0/rules_python-0.21.0.tar.gz",
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

        strip_prefix = "rules_nixpkgs-%s" % _rules_nixpkgs_version

        rules_nixpkgs_url = \
            "https://github.com/tweag/rules_nixpkgs/releases/download/v{version}/{prefix}.tar.gz".format(
                version = _rules_nixpkgs_version,
                prefix = strip_prefix,
            )

        http_archive(
            name = "io_tweag_rules_nixpkgs",
            strip_prefix = strip_prefix,
            urls = [rules_nixpkgs_url],
            sha256 = _rules_nixpkgs_sha256,
        )

        # required by rules_nixpkgs
        http_archive(
            name = "rules_nodejs",
            sha256 = "8fc8e300cb67b89ceebd5b8ba6896ff273c84f6099fc88d23f24e7102319d8fd",
            urls = ["https://github.com/bazelbuild/rules_nodejs/releases/download/5.8.4/rules_nodejs-core-5.8.4.tar.gz"],
        )

        http_archive(
            name = "rules_nixpkgs_core",
            strip_prefix = strip_prefix + "/core",
            urls = [rules_nixpkgs_url],
            sha256 = _rules_nixpkgs_sha256,
        )

        for toolchain in ["cc", "java", "python", "go", "rust", "posix", "nodejs"]:
            http_archive(
                name = "rules_nixpkgs_" + toolchain,
                strip_prefix = strip_prefix + "/toolchains/" + toolchain,
                urls = [rules_nixpkgs_url],
                sha256 = _rules_nixpkgs_sha256,
            )

    maybe(
        http_archive,
        name = "com_google_protobuf",
        sha256 = "22fdaf641b31655d4b2297f9981fa5203b2866f8332d3c6333f6b0107bb320de",
        strip_prefix = "protobuf-21.12",
        urls = [
            "https://github.com/protocolbuffers/protobuf/archive/refs/tags/v21.12.tar.gz",
        ],
    )

    maybe(
        http_archive,
        name = "aspect_rules_js",
        sha256 = "d9ceb89e97bb5ad53b278148e01a77a3e9100db272ce4ebdcd59889d26b9076e",
        strip_prefix = "rules_js-1.34.0",
        url = "https://github.com/aspect-build/rules_js/releases/download/v1.34.0/rules_js-v1.34.0.tar.gz",
    )

    rules_haskell_dependencies_bzlmod()

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
        sha256 = "b5b06d60ce49c8ba700e0ba517fa07de80b5d4628a037f4be8ad16955be7a7c0",
        strip_prefix = "zlib-1.3",
        urls = ["https://github.com/madler/zlib/archive/v1.3.tar.gz"],
    )
    maybe(
        http_archive,
        name = "rules_pkg",
        urls = [
            "https://mirror.bazel.build/github.com/bazelbuild/rules_pkg/releases/download/0.9.1/rules_pkg-0.9.1.tar.gz",
            "https://github.com/bazelbuild/rules_pkg/releases/download/0.9.1/rules_pkg-0.9.1.tar.gz",
        ],
        sha256 = "8f9ee2dc10c1ae514ee599a8b42ed99fa262b757058f65ad3c384289ff70c4b8",
    )

    # For --incompatible_disable_starlark_host_transitions support (default in bazel 7)
    # Temporarily overrides the rules_licence that comes with bazel to workaround
    # https://github.com/bazelbuild/bazel/issues/17032#issuecomment-1548459728
    maybe(
        http_archive,
        name = "rules_license",
        sha256 = "4531deccb913639c30e5c7512a054d5d875698daeb75d8cf90f284375fe7c360",
        urls = [
            "https://mirror.bazel.build/github.com/bazelbuild/rules_license/releases/download/0.0.7/rules_license-0.0.7.tar.gz",
            "https://github.com/bazelbuild/rules_license/releases/download/0.0.7/rules_license-0.0.7.tar.gz",
        ],
    )

def haskell_repositories():
    """Alias for rules_haskell_dependencies

    Deprecated:
      Use rules_haskell_dependencies instead.
    """
    rules_haskell_dependencies()
