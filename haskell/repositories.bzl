"""Workspace rules (repositories)"""

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@bazel_tools//tools/build_defs/repo:utils.bzl", "maybe")
load(
    ":private/versions.bzl",
    "check_bazel_version_compatible",
)
load(":private/ghc_ci.bzl", "ghc_default_version")

_rules_nixpkgs_version = "0.11.1"
_rules_nixpkgs_sha256 = "2a555348d7f8593fca2bf3fc6ce53c5d62929de81b6c292e23f16c557c0ae45a"

_rules_sh_version = "v0.4.0"
_rules_sh_sha256 = "3243af3fcb3768633fd39f3654de773e5fb61471a2fae5762a1653c22c412d2c"

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
            "https://mirror.bazel.build/github.com/bazelbuild/platforms/releases/download/0.0.9/platforms-0.0.9.tar.gz",
            "https://github.com/bazelbuild/platforms/releases/download/0.0.9/platforms-0.0.9.tar.gz",
        ],
        sha256 = "5eda539c841265031c2f82d8ae7a3a6490bd62176e0c038fc469eabf91f6149b",
    )

    maybe(
        http_archive,
        name = "bazel_skylib",
        urls = [
            "https://mirror.bazel.build/github.com/bazelbuild/bazel-skylib/releases/download/1.6.1/bazel-skylib-1.6.1.tar.gz",
            "https://github.com/bazelbuild/bazel-skylib/releases/download/1.6.1/bazel-skylib-1.6.1.tar.gz",
        ],
        sha256 = "9f38886a40548c6e96c106b752f242130ee11aaa068a56ba7e56f4511f33e4f2",
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
        urls = [
            "https://github.com/tweag/rules_sh/releases/download/{TAG}/rules_sh-{VERSION}.tar.gz".format(
                TAG = _rules_sh_version,
                VERSION = _rules_sh_version.lstrip("v"),
            ),
        ],
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
        sha256 = "bc9b4a01ef8eb050d8a7a050eedde8ffb1e45a56b0e4094e26f06c17d5fcf1d5",
        strip_prefix = "rules_js-1.41.2",
        url = "https://github.com/aspect-build/rules_js/releases/download/v1.41.2/rules_js-v1.41.2.tar.gz",
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
        sha256 = "9a93b2b7dfdac77ceba5a558a580e74667dd6fede4585b91eefb60f03b72df23",
        strip_prefix = "zlib-1.3.1",
        urls = ["https://github.com/madler/zlib/releases/download/v1.3.1/zlib-1.3.1.tar.gz"],
    )
    maybe(
        http_archive,
        name = "rules_pkg",
        urls = [
            "https://mirror.bazel.build/github.com/bazelbuild/rules_pkg/releases/download/0.10.1/rules_pkg-0.10.1.tar.gz",
            "https://github.com/bazelbuild/rules_pkg/releases/download/0.10.1/rules_pkg-0.10.1.tar.gz",
        ],
        sha256 = "d250924a2ecc5176808fc4c25d5cf5e9e79e6346d79d5ab1c493e289e722d1d0",
    )

    # For --incompatible_disable_starlark_host_transitions support (default in bazel 7)
    # Temporarily overrides the rules_licence that comes with bazel to workaround
    # https://github.com/bazelbuild/bazel/issues/17032#issuecomment-1548459728
    maybe(
        http_archive,
        name = "rules_license",
        sha256 = "241b06f3097fd186ff468832150d6cc142247dc42a32aaefb56d0099895fd229",
        urls = [
            "https://mirror.bazel.build/github.com/bazelbuild/rules_license/releases/download/0.0.8/rules_license-0.0.8.tar.gz",
            "https://github.com/bazelbuild/rules_license/releases/download/0.0.8/rules_license-0.0.8.tar.gz",
        ],
    )

def haskell_repositories():
    """Alias for rules_haskell_dependencies

    Deprecated:
      Use rules_haskell_dependencies instead.
    """
    rules_haskell_dependencies()
