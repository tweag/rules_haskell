workspace(name = "rules_haskell")

load("//haskell:repositories.bzl", "rules_haskell_dependencies")

rules_haskell_dependencies()

load("//:non_module_deps.bzl", "repositories")

repositories(bzlmod = False)

load("@os_info//:os_info.bzl", "is_linux", "is_nix_shell", "is_windows")

# bazel dependencies
load("@aspect_rules_js//js:repositories.bzl", "rules_js_dependencies")

rules_js_dependencies()

load("@bazel_skylib//:workspace.bzl", "bazel_skylib_workspace")

bazel_skylib_workspace()

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

# TODO: Remove when tests are run with a ghc version containing Cabal >= 3.10
# See https://github.com/tweag/rules_haskell/issues/1871
http_archive(
    name = "Cabal",
    build_file_content = """
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_library")
haskell_cabal_library(
    name = "Cabal",
    srcs = glob(["Cabal/**"]),
    verbose = False,
    version = "3.6.3.0",
    visibility = ["//visibility:public"],
)
    """,
    sha256 = "f69b46cb897edab3aa8d5a4bd7b8690b76cd6f0b320521afd01ddd20601d1356",
    strip_prefix = "cabal-gg-8220-with-3630",
    urls = ["https://github.com/tweag/cabal/archive/refs/heads/gg/8220-with-3630.zip"],
)

http_archive(
    name = "alex",
    build_file_content = """
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_library", "haskell_cabal_binary")

haskell_cabal_library(
    name = "alex-lib",
    setup_deps = ["@Cabal//:Cabal"],
    srcs = glob(["**"]),
    version = "3.2.7.1",
    visibility = ["//visibility:public"],
)

haskell_cabal_binary(
    name = "alex",
    setup_deps = ["@Cabal//:Cabal"],
    srcs = glob(["**"]),
    verbose = False,
    visibility = ["//visibility:public"],
)
    """,
    sha256 = "9bd2f1a27e8f1b2ffdb5b2fbd3ed82b6f0e85191459a1b24ffcbef4e68a81bec",
    strip_prefix = "alex-3.2.7.1",
    urls = ["http://hackage.haskell.org/package/alex-3.2.7.1/alex-3.2.7.1.tar.gz"],
)

load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")

stack_snapshot(
    name = "stackage",
    components = {
        "alex": [],
        "attoparsec": [
            # attoparsec contains an internal library which is not exposed publicly,
            # but required to build the public library, hence the declaration of
            # those 2 components, as well as the explicit declaration of the
            # dependency between them.
            "lib",
            "lib:attoparsec-internal",
        ],
        "proto-lens-protoc": [
            "lib",
            "exe",
        ],
    },
    components_dependencies = {
        "attoparsec": """{"lib:attoparsec": ["lib:attoparsec-internal"]}""",
    },
    local_snapshot = "//:stackage_snapshot.yaml",
    packages = [
        # Core libraries
        "alex",
        "array",
        "base",
        "bytestring",
        "c2hs",
        "conduit",
        "conduit-extra",
        "containers",
        "deepseq",
        "directory",
        "filepath",
        "ghc-heap",
        "happy",
        "mtl",
        "optparse-applicative",
        "process",
        "text",
        "text-show",
        "vector",
        # For tests
        "cabal-doctest",
        "doctest",
        "polysemy",
        "network",
        "language-c",
        "streaming",
        "void",
        "ghc-check",
        "hspec",
        "hspec-core",
        "lens-family-core",
        "data-default-class",
        "profunctors",
        "proto-lens",
        "proto-lens-protoc",
        "proto-lens-runtime",
        "lens-family",
        "safe-exceptions",
        "temporary",
    ],
    setup_deps = {
        "polysemy": ["cabal-doctest"],
        # See https://github.com/tweag/rules_haskell/issues/1871
        "HUnit": ["@Cabal//:Cabal"],
        "bifunctors": ["@Cabal//:Cabal"],
        "c2hs": ["@Cabal//:Cabal"],
        "call-stack": ["@Cabal//:Cabal"],
        "doctest": ["@Cabal//:Cabal"],
        "generic-deriving": ["@Cabal//:Cabal"],
        "happy": ["@Cabal//:Cabal"],
        "hspec": ["@Cabal//:Cabal"],
        "hspec-core": ["@Cabal//:Cabal"],
        "hspec-discover": ["@Cabal//:Cabal"],
        "hspec-expectations": ["@Cabal//:Cabal"],
        "mono-traversable": ["@Cabal//:Cabal"],
        "proto-lens-protoc": ["@Cabal//:Cabal"],
        "proto-lens-runtime": ["@Cabal//:Cabal"],
        "quickcheck-io": ["@Cabal//:Cabal"],
        "transformers-compat": ["@Cabal//:Cabal"],
        "type-errors": ["@Cabal//:Cabal"],
        "typed-process": ["@Cabal//:Cabal"],
        "unliftio-core": ["@Cabal//:Cabal"],
    },
    stack_snapshot_json = "//:stackage_snapshot.json" if not is_windows else None,
    tools = [
        # This is not required, as `stack_snapshot` would build alex
        # automatically, however it is used as a test for user provided
        # `tools`. We also override alex's components to avoid building it
        # twice.
        "@alex",
    ],
    vendored_packages = {
        "ghc-paths": "@rules_haskell//tools/ghc-paths",
    },
)

# In a separate repo because not all platforms support zlib.
stack_snapshot(
    name = "stackage-zlib",
    extra_deps = {"zlib": ["//tests:zlib"]},
    local_snapshot = "//:stackage_snapshot.yaml",
    packages = ["zlib"],
    stack_snapshot_json = "//:stackage-zlib-snapshot.json" if not is_windows else None,
)

stack_snapshot(
    name = "stackage-pinning-test",
    components = {
        "package1": [
            "lib:package1",
            "lib:sublib",
        ],
    },
    components_dependencies = {
        "package1": """{"lib:package1": ["lib:sublib"]}""",
    },
    local_snapshot = "//:stackage-pinning-test.yaml",
    packages = [
        "hspec",
        "package1",
    ],
    setup_deps = {
        # See https://github.com/tweag/rules_haskell/issues/1871
        "HUnit": ["@Cabal//:Cabal"],
        "call-stack": ["@Cabal//:Cabal"],
        "hspec": ["@Cabal//:Cabal"],
        "hspec-core": ["@Cabal//:Cabal"],
        "hspec-discover": ["@Cabal//:Cabal"],
        "hspec-expectations": ["@Cabal//:Cabal"],
        "quickcheck-io": ["@Cabal//:Cabal"],
    },
    stack_snapshot_json = "//:stackage-pinning-test_snapshot.json" if not is_windows else None,
)

# Vendor data-default-instances-containers and data-default-instances-old-local
# to work around build failures due to file paths exceeding `MAX_PATH` on
# Windows.
#
#   ghc.exe: loadObj: C:\Users\runneradmin\_bazel_runneradmin\minshlu6\execroot\rules_haskell\bazel-out\x64_windows-fastbuild\bin\external\ghcide\data-default-instances-containers-0.0.1\_install\data-default-instances-containers-0.0.1_iface\HSdata-default-instances-containers-0.0.1.o: file doesn't exist
#
# Recent versions of GHC fix many `MAX_PATH` issues on Windows. However, GHC's
# loader still exposes such an issue. A fix has been merged in GHC HEAD, but is
# not included in GHC 8.10.4 or 9.0.1.
# See https://gitlab.haskell.org/ghc/ghc/-/issues/19541
http_archive(
    name = "data-default-ic",
    build_file_content = """
load("@rules_haskell//haskell:defs.bzl", "haskell_library")
load("@ghcide//:packages.bzl", "packages")
package_name = "data-default-instances-containers"
haskell_library(
    name = "lib",
    package_name = package_name,
    version = packages[package_name].version,
    srcs = glob(["**/*.hs"]),
    deps = packages[package_name].deps,
    visibility = ["//visibility:public"],
)
    """,
    sha256 = "a55e07af005c9815d82f3fc95e125db82994377c9f4a769428878701d4ec081a",
    strip_prefix = "data-default-instances-containers-0.0.1",
    urls = [
        "https://hackage.haskell.org/package/data-default-instances-containers-0.0.1/data-default-instances-containers-0.0.1.tar.gz",
        "https://s3.amazonaws.com/hackage.fpcomplete.com/package/data-default-instances-containers-0.0.1.tar.gz",
    ],
)

http_archive(
    name = "data-default-ol",
    build_file_content = """
load("@rules_haskell//haskell:defs.bzl", "haskell_library")
load("@ghcide//:packages.bzl", "packages")
package_name = "data-default-instances-old-locale"
haskell_library(
    name = "lib",
    package_name = package_name,
    version = packages[package_name].version,
    srcs = glob(["**/*.hs"]),
    deps = packages[package_name].deps,
    visibility = ["//visibility:public"],
)
    """,
    sha256 = "60d3b02922958c4908d7bf2b24ddf61511665745f784227d206745784b0c0802",
    strip_prefix = "data-default-instances-old-locale-0.0.1",
    urls = [
        "https://hackage.haskell.org/package/data-default-instances-old-locale-0.0.1/data-default-instances-old-locale-0.0.1.tar.gz",
        "https://s3.amazonaws.com/hackage.fpcomplete.com/package/data-default-instances-old-locale-0.0.1.tar.gz",
    ],
)

stack_snapshot(
    name = "ghcide",
    components = {
        "ghcide": [
            "lib",
            "exe",
        ],
        "attoparsec": [
            "lib",
            "lib:attoparsec-internal",
        ],
    },
    components_dependencies = {
        "attoparsec": """{"lib:attoparsec": ["lib:attoparsec-internal"]}""",
    },
    extra_deps = {"zlib": ["//tests:zlib"]},
    haddock = False,
    local_snapshot = "//:ghcide-stack-snapshot.yaml",
    packages = [
        "ghcide",
    ],
    setup_deps = {
        "bifunctors": ["@ghcide//:Cabal"],
        "call-stack": ["@ghcide//:Cabal"],
        "ghcide": ["@ghcide//:Cabal"],
        "hie-bios": ["@ghcide//:Cabal"],
        "hls-graph": ["@ghcide//:Cabal"],
        "hspec-discover": ["@ghcide//:Cabal"],
        "implicit-hie": ["@ghcide//:Cabal"],
        "implicit-hie-cradle": ["@ghcide//:Cabal"],
        "invariant": ["@ghcide//:Cabal"],
        "js-dgtable": ["@ghcide//:Cabal"],
        "js-flot": ["@ghcide//:Cabal"],
        "js-jquery": ["@ghcide//:Cabal"],
        "libyaml": ["@ghcide//:Cabal"],
        "mono-traversable": ["@ghcide//:Cabal"],
        "regex-base": ["@ghcide//:Cabal"],
        "regex-tdfa": ["@ghcide//:Cabal"],
        "transformers-compat": ["@ghcide//:Cabal"],
        "typed-process": ["@ghcide//:Cabal"],
        "unliftio": ["@ghcide//:Cabal"],
        "unliftio-core": ["@ghcide//:Cabal"],
        "yaml": ["@ghcide//:Cabal"],
    },
    stack_snapshot_json = "//:ghcide-snapshot.json" if not is_windows else None,
    vendored_packages = {
        "data-default-instances-containers": "@data-default-ic//:lib",
        "data-default-instances-old-locale": "@data-default-ol//:lib",
        "ghc-paths": "@rules_haskell//tools/ghc-paths",
    },
)

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
# starting from 0.29, rules_go requires bazel >= 4.2.0
http_archive(
    name = "io_bazel_rules_go",
    sha256 = "8e968b5fcea1d2d64071872b12737bbb5514524ee5f0a4f54f5920266c261acb",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/rules_go/releases/download/v0.28.0/rules_go-v0.28.0.zip",
        "https://github.com/bazelbuild/rules_go/releases/download/v0.28.0/rules_go-v0.28.0.zip",
    ],
)

load("non_module_deps_1.bzl", repositories_1 = "repositories")

repositories_1(bzlmod = False)

load("@rules_haskell_npm//:repositories.bzl", "npm_repositories")

npm_repositories()

register_toolchains(
    "//tests:c2hs-toolchain",
    "//tests:doctest-toolchain",
    "//tests:protobuf-toolchain",
    "//tests:protobuf-toolchain-osx_arm64",
)

load("@bazel_tools//tools/build_defs/repo:jvm.bzl", "jvm_maven_import_external")

jvm_maven_import_external(
    name = "org_apache_spark_spark_core_2_10",
    artifact = "org.apache.spark:spark-core_2.10:1.6.0",
    artifact_sha256 = "28aad0602a5eea97e9cfed3a7c5f2934cd5afefdb7f7c1d871bb07985453ea6e",
    licenses = ["notice"],
    server_urls = ["https://repo.maven.apache.org/maven2"],
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

# If in nix-shell, use the Go SDK provided by Nix.
# Otherwise, ask Bazel to download a Go SDK.
go_register_toolchains(version = "host") if is_nix_shell else go_register_toolchains(version = "1.16.2")

load("@com_github_bazelbuild_buildtools//buildifier:deps.bzl", "buildifier_dependencies")

buildifier_dependencies()

load("@contrib_rules_bazel_integration_test//bazel_integration_test:deps.bzl", "bazel_integration_test_rules_dependencies")

bazel_integration_test_rules_dependencies()

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


load("@toolchains_libraries//:toolchain_libraries.bzl", "toolchain_libraries")

stack_snapshot(
    name = "stackage_asterius",
    local_snapshot = "@rules_haskell//tests/asterius/stack_toolchain_libraries:snapshot.yaml",
    packages = [
        "xhtml",
    ],
    stack_snapshot_json = "@rules_haskell//tests/asterius/stack_toolchain_libraries:snapshot.json",
    toolchain_libraries = toolchain_libraries,
) if is_linux else None
