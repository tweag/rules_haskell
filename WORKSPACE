workspace(name = "rules_haskell")

load("//haskell:repositories.bzl", "rules_haskell_dependencies")

rules_haskell_dependencies()

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
    name = "io_bazel_stardoc",
    sha256 = "3fd8fec4ddec3c670bd810904e2e33170bedfe12f90adf943508184be458c8bb",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/stardoc/releases/download/0.5.3/stardoc-0.5.3.tar.gz",
        "https://github.com/bazelbuild/stardoc/releases/download/0.5.3/stardoc-0.5.3.tar.gz",
    ],
)

load("@aspect_rules_js//js:repositories.bzl", "rules_js_dependencies")

rules_js_dependencies()

load("@bazel_skylib//:workspace.bzl", "bazel_skylib_workspace")

bazel_skylib_workspace()

http_archive(
    name = "rules_proto",
    sha256 = "36476f17a78a4c495b9a9e70bd92d182e6e78db476d90c74bac1f5f19f0d6d04",
    strip_prefix = "rules_proto-fcad4680fee127dbd8344e6a961a28eef5820ef4",
    urls = ["https://github.com/bazelbuild/rules_proto/archive/fcad4680fee127dbd8344e6a961a28eef5820ef4.tar.gz"],
)

load("@rules_proto//proto:repositories.bzl", "rules_proto_dependencies", "rules_proto_toolchains")

rules_proto_dependencies()

rules_proto_toolchains()

# For buildifier
http_archive(
    name = "io_bazel_rules_go",
    sha256 = "6dc2da7ab4cf5d7bfc7c949776b1b7c733f05e56edc4bcd9022bb249d2e2a996",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/rules_go/releases/download/v0.39.1/rules_go-v0.39.1.zip",
        "https://github.com/bazelbuild/rules_go/releases/download/v0.39.1/rules_go-v0.39.1.zip",
    ],
)

load("//:non_module_dev_deps.bzl", "repositories")

repositories(bzlmod = False)

load("//:non_module_dev_deps_2.bzl", _repositories_2 = "repositories")

_repositories_2(bzlmod = False)

load(
    "@rules_haskell//haskell/asterius:repositories.bzl",
    "asterius_dependencies_bindist",
    "asterius_dependencies_nix",
)
load(
    "@rules_nixpkgs_core//:nixpkgs.bzl",
    "nixpkgs_package",
)
load("@os_info//:os_info.bzl", "is_nix_shell")

asterius_dependencies_nix(
    nix_repository = "@nixpkgs_default",
    nixpkgs_package_rule = nixpkgs_package,
) if is_nix_shell else asterius_dependencies_bindist()

load("@rules_haskell_npm//:repositories.bzl", "npm_repositories")

npm_repositories()

register_toolchains(
    "//tests:protobuf-toolchain",
    "//tests:protobuf-toolchain-osx_arm64",
)

load("@io_bazel_stardoc//:setup.bzl", "stardoc_repositories")

stardoc_repositories()

register_toolchains(
    "@rules_haskell//docs/pandoc:nixpkgs",
    "@rules_haskell//docs/pandoc:linux",
    "@rules_haskell//docs/pandoc:macos",
)

# A repository that generates the Go SDK imports, see ./tools/go_sdk/README
local_repository(
    name = "go_sdk_repo",
    path = "tools/go_sdk",
)

load(
    "@io_bazel_rules_go//go:deps.bzl",
    "go_register_toolchains",
    "go_rules_dependencies",
)

go_rules_dependencies()

go_register_toolchains(version = "1.20.2")

load("@com_github_bazelbuild_buildtools//buildifier:deps.bzl", "buildifier_dependencies")

buildifier_dependencies()

http_archive(
    name = "cgrindel_bazel_starlib",
    sha256 = "ee0033d029b5eaddc21836b2944cf37c95eb5f214eb39834136a316dbc252a73",
    urls = [
        "https://github.com/cgrindel/bazel-starlib/releases/download/v0.16.0/bazel-starlib.v0.16.0.tar.gz",
    ],
)

load("@cgrindel_bazel_starlib//:deps.bzl", "bazel_starlib_dependencies")

bazel_starlib_dependencies()

# For profiling
# Required to make use of `bazel build --profile`.
# Dummy target //external:python_headers.
# See https://github.com/protocolbuffers/protobuf/blob/d9ccd0c0e6bbda9bf4476088eeb46b02d7dcd327/util/python/BUILD
bind(
    name = "python_headers",
    actual = "@com_google_protobuf//util/python:python_headers",
)
