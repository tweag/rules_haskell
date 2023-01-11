workspace(name = "rules_haskell")

# Subrepositories of rules_haskell

# Some helpers for platform-dependent configuration
load("//tools:os_info.bzl", "os_info")

os_info(name = "os_info")

load("@os_info//:os_info.bzl", "is_linux", "is_nix_shell", "is_windows")

# bazel dependencies
load("//haskell:repositories.bzl", "rules_haskell_dependencies")

rules_haskell_dependencies()

load("@build_bazel_rules_nodejs//:repositories.bzl", "build_bazel_rules_nodejs_dependencies")

build_bazel_rules_nodejs_dependencies()

load("@bazel_skylib//:workspace.bzl", "bazel_skylib_workspace")

bazel_skylib_workspace()

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
    name = "alex",
    build_file_content = """
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_binary")
haskell_cabal_binary(
    name = "alex",
    srcs = glob(["**"]),
    verbose = False,
    visibility = ["//visibility:public"],
)
    """,
    sha256 = "91aa08c1d3312125fbf4284815189299bbb0be34421ab963b1f2ae06eccc5410",
    strip_prefix = "alex-3.2.6",
    urls = ["http://hackage.haskell.org/package/alex-3.2.6/alex-3.2.6.tar.gz"],
)

load(
    "@rules_haskell//:constants.bzl",
    "test_asterius_version",
    "test_ghc_version",
)
load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")

stack_snapshot(
    name = "stackage",
    components = {
        "alex": [],
        "proto-lens-protoc": [
            "lib",
            "exe",
        ],
    },
    local_snapshot = "//:stackage_snapshot.yaml",
    packages = [
        # Core libraries
        "array",
        "base",
        "bytestring",
        "containers",
        "deepseq",
        "directory",
        "filepath",
        "ghc-heap",
        "mtl",
        "process",
        "text",
        "vector",
        # For tests
        "c2hs",
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
        "profunctors-5.5.2",
        "proto-lens-0.7.0.0",
        "proto-lens-protoc-0.7.0.0",
        "proto-lens-runtime-0.7.0.0",
        "lens-family",
        "safe-exceptions",
        "temporary",
    ],
    setup_deps = {"polysemy": ["cabal-doctest"]},
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
    components = {"ghcide": [
        "lib",
        "exe",
    ]},
    extra_deps = {"zlib": ["//tests:zlib"]},
    haddock = False,
    local_snapshot = "//:ghcide-stack-snapshot.yaml",
    packages = [
        "ghcide",
    ],
    stack_snapshot_json = "//:ghcide-snapshot.json" if not is_windows else None,
    vendored_packages = {
        "data-default-instances-containers": "@data-default-ic//:lib",
        "data-default-instances-old-locale": "@data-default-ol//:lib",
        "ghc-paths": "@rules_haskell//tools/ghc-paths",
    },
)

load(
    "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_cc_configure",
    "nixpkgs_local_repository",
    "nixpkgs_package",
    "nixpkgs_python_configure",
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

nixpkgs_local_repository(
    name = "nixpkgs_default",
    nix_file = "//nixpkgs:default.nix",
)

test_ghcopts = [
    "-XStandaloneDeriving",  # Flag used at compile time
    "-threaded",  # Flag used at link time

    # Used by `tests/repl-flags`
    "-DTESTS_TOOLCHAIN_COMPILER_FLAGS",
    # this is the default, so it does not harm other tests
    "-XNoOverloadedStrings",
]

test_haddock_flags = ["-U"]

test_repl_ghci_args = [
    # The repl test will need this flag, but set by the local
    # `repl_ghci_args`.
    "-UTESTS_TOOLCHAIN_REPL_FLAGS",
    # The repl test will need OverloadedString
    "-XOverloadedStrings",
]

test_cabalopts = [
    # Used by `tests/cabal-toolchain-flags`
    "--ghc-option=-DTESTS_TOOLCHAIN_CABALOPTS",
    "--haddock-option=--optghc=-DTESTS_TOOLCHAIN_CABALOPTS",
] + ([
    # To avoid ghcide linking errors with heapsize on Windows of the form
    #
    #   unknown symbol `heap_view_closurePtrs'
    #
    # See https://github.com/haskell/ghcide/pull/954
    "--disable-library-for-ghci",
] if is_windows else [])

load(
    "@rules_haskell//haskell:nixpkgs.bzl",
    "haskell_register_ghc_nixpkgs",
)

haskell_register_ghc_nixpkgs(
    attribute_path = "",
    cabalopts = test_cabalopts,
    ghcopts = test_ghcopts,
    haddock_flags = test_haddock_flags,
    locale_archive = "@glibc_locales//:locale-archive",
    nix_file_content = """with import <nixpkgs> {}; haskell.packages.ghc8107.ghc""",
    repl_ghci_args = test_repl_ghci_args,
    repository = "@nixpkgs_default",
    version = test_ghc_version,
)

load(
    "//haskell/asterius:repositories.bzl",
    "asterius_dependencies_bindist",
    "asterius_dependencies_nix",
    "rules_haskell_asterius_toolchains",
    "toolchain_libraries",
)

(asterius_dependencies_nix(
    nix_repository = "@nixpkgs_default",
    nixpkgs_package_rule = nixpkgs_package,
) if is_nix_shell else asterius_dependencies_bindist())

rules_haskell_asterius_toolchains(
    cabalopts = test_cabalopts,
    ghcopts = test_ghcopts,
    repl_ghci_args = test_repl_ghci_args,
    version = test_asterius_version,
)

load(
    "@rules_haskell//haskell:ghc_bindist.bzl",
    "haskell_register_ghc_bindists",
)

haskell_register_ghc_bindists(
    cabalopts = test_cabalopts,
    ghcopts = test_ghcopts,
    version = test_ghc_version,
)

register_toolchains(
    "//tests:c2hs-toolchain",
    "//tests:doctest-toolchain",
    "//tests:protobuf-toolchain",
    "//tests:protobuf-toolchain-osx_arm64",
)

nixpkgs_cc_configure(
    # Don't override the default cc toolchain needed for bindist mode.
    name = "nixpkgs_config_cc",
    repository = "@nixpkgs_default",
)

nixpkgs_python_configure(repository = "@nixpkgs_default")

nixpkgs_package(
    name = "nixpkgs_zlib",
    attribute_path = "zlib",
    repository = "@nixpkgs_default",
)

nixpkgs_package(
    name = "nixpkgs_lz4",
    attribute_path = "lz4.out",
    build_file_content = """
package(default_visibility = ["//visibility:public"])
load("@rules_cc//cc:defs.bzl", "cc_library")

cc_library(
  name = "nixpkgs_lz4",
  srcs = glob(["lib/liblz4.dylib", "lib/liblz4.so*"], allow_empty = True),
  includes = ["include"],
)
    """,
    repository = "@nixpkgs_default",
)

nixpkgs_package(
    name = "python3",
    repository = "@nixpkgs_default",
)

nixpkgs_package(
    name = "sphinx",
    attribute_path = "python39Packages.sphinx",
    repository = "@nixpkgs_default",
)

nixpkgs_package(
    name = "graphviz",
    attribute_path = "graphviz",
    repository = "@nixpkgs_default",
)

nixpkgs_package(
    name = "zip",
    attribute_path = "zip",
    repository = "@nixpkgs_default",
)

nixpkgs_package(
    name = "zlib.dev",
    build_file_content = """
load("@rules_cc//cc:defs.bzl", "cc_library")

filegroup(
    name = "include",
    srcs = glob(["include/*.h"]),
    visibility = ["//visibility:public"],
)

cc_library(
    name = "zlib",
    srcs = ["@nixpkgs_zlib//:lib"],
    hdrs = [":include"],
    strip_include_prefix = "include",
    visibility = ["//visibility:public"],
    # This rule only bundles headers and a library and doesn't compile or link by itself.
    # We set linkstatic = 1 to quiet to quiet the following warning:
    #
    #   in linkstatic attribute of cc_library rule @zlib.dev//:zlib:
    #   setting 'linkstatic=1' is recommended if there are no object files.
    #
    linkstatic = 1,
)
""",
    repository = "@nixpkgs_default",
)

nixpkgs_package(
    name = "glibc_locales",
    attribute_path = "glibcLocales",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

filegroup(
    name = "locale-archive",
    srcs = ["lib/locale/locale-archive"],
)
""",
    repository = "@nixpkgs_default",
)

http_archive(
    name = "zlib.hs",
    build_file_content = """
load("@os_info//:os_info.bzl", "is_darwin")
load("@rules_cc//cc:defs.bzl", "cc_library")
cc_library(
    name = "zlib",
    # Import `:z` as `srcs` to enforce the library name `libz.so`. Otherwise,
    # Bazel would mangle the library name and e.g. Cabal wouldn't recognize it.
    srcs = [":z"],
    hdrs = glob(["*.h"]),
    includes = ["."],
    linkstatic = 1,
    visibility = ["//visibility:public"],
)
cc_library(
    name = "z",
    srcs = glob(["*.c"]),
    hdrs = glob(["*.h"]),
    # Needed because XCode 12.0 Clang errors by default.
    # See https://developer.apple.com/documentation/xcode-release-notes/xcode-12-release-notes.
    copts = ["-Wno-error=implicit-function-declaration"],
    # Cabal packages depending on dynamic C libraries fail on MacOS
    # due to `-rpath` flags being forwarded indiscriminately.
    # See https://github.com/tweag/rules_haskell/issues/1317
    linkstatic = is_darwin,
)
""",
    sha256 = "c3e5e9fdd5004dcb542feda5ee4f0ff0744628baf8ed2dd5d66f8ca1197cb1a1",
    strip_prefix = "zlib-1.2.11",
    urls = [
        "https://mirror.bazel.build/zlib.net/zlib-1.2.11.tar.gz",
        "http://zlib.net/zlib-1.2.11.tar.gz",
    ],
)

load("@bazel_tools//tools/build_defs/repo:jvm.bzl", "jvm_maven_import_external")

jvm_maven_import_external(
    name = "org_apache_spark_spark_core_2_10",
    artifact = "org.apache.spark:spark-core_2.10:1.6.0",
    artifact_sha256 = "28aad0602a5eea97e9cfed3a7c5f2934cd5afefdb7f7c1d871bb07985453ea6e",
    licenses = ["notice"],
    server_urls = ["https://repo.maven.apache.org/maven2"],
)

# c2hs rule in its own repository
local_repository(
    name = "c2hs_repo",
    path = "tests/c2hs/repo",
)

# haskell_library rule in its own repository
local_repository(
    name = "library_repo",
    path = "tests/library-external-workspace/repo",
)

load(
    "@rules_haskell//tests/external-haskell-repository:workspace_dummy.bzl",
    "haskell_package_repository_dummy",
)

# dummy repo for the external haskell repo test
haskell_package_repository_dummy(
    name = "haskell_package_repository_dummy",
)

http_archive(
    name = "io_bazel_stardoc",
    sha256 = "3fd8fec4ddec3c670bd810904e2e33170bedfe12f90adf943508184be458c8bb",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/stardoc/releases/download/0.5.3/stardoc-0.5.3.tar.gz",
        "https://github.com/bazelbuild/stardoc/releases/download/0.5.3/stardoc-0.5.3.tar.gz",
    ],
)

load("@io_bazel_stardoc//:setup.bzl", "stardoc_repositories")

stardoc_repositories()

load(
    "@rules_haskell//docs/pandoc:pandoc.bzl",
    "import_pandoc_bindists",
    "nixpkgs_pandoc_configure",
)

nixpkgs_pandoc_configure(repository = "@nixpkgs_default")

import_pandoc_bindists()

register_toolchains(
    "@rules_haskell//docs/pandoc:nixpkgs",
    "@rules_haskell//docs/pandoc:linux",
    "@rules_haskell//docs/pandoc:macos",
)

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

http_archive(
    name = "com_github_bazelbuild_buildtools",
    sha256 = "614c84128ddb86aab4e1f25ba2e027d32fd5c6da302ae30685b9d7973b13da1b",
    strip_prefix = "buildtools-4.2.3",
    urls = ["https://github.com/bazelbuild/buildtools/archive/4.2.3.tar.gz"],
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

http_archive(
    name = "contrib_rules_bazel_integration_test",
    sha256 = "f80c4052df80e9099ed0f2f27ef4084604333566a7b028f524ceae6e5569b429",
    strip_prefix = "rules_bazel_integration_test-7ee995a20bbaa2f6540103c63ff4891166133c2f",
    urls = [
        "https://github.com/bazel-contrib/rules_bazel_integration_test/archive/7ee995a20bbaa2f6540103c63ff4891166133c2f.zip",
    ],
)

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

# For persistent worker (tools/worker)
load("//tools:repositories.bzl", "rules_haskell_worker_dependencies")

rules_haskell_worker_dependencies()

# Stack snapshot repository for testing non standard toolchains
# The toolchain_libraries rule provide a default value for the toolchain_libraries
# variable, so we can load it even if we are not on linux.

toolchain_libraries(
    name = "toolchains_libraries",
    repository = "linux_amd64_asterius" if is_linux else "",
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

load(
    "//tests/integration_testing:dependencies.bzl",
    "integration_testing_bazel_binaries",
)

integration_testing_bazel_binaries()

local_repository(
    name = "tutorial",
    path = "tutorial",
)

local_repository(
    name = "examples",
    path = "examples",
)

local_repository(
    name = "examples-arm",
    path = "examples/arm",
)
