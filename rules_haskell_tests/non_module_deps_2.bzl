""" External repositories for the CI that need to be shared between WORKSPACE and MODULE.bazel files """

load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")
load("@os_info//:os_info.bzl", "is_linux", "is_windows")
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@toolchains_libraries//:toolchain_libraries.bzl", "toolchain_libraries")

label_builder = lambda x: Label(x)

def _empty_repo_impl(rctx):
    fail(rctx.attrs.error_msg)

_empty_repo = repository_rule(
    implementation = _empty_repo_impl,
    doc = """A dummy repository that can be loaded from the MODULE.bazel file but not fetched.""",
    attrs = {
        "error_msg": attr.string(
            doc = "The error message displayed if the repository is fetched",
            mandatory = True,
        ),
    },
)

def repositories(*, bzlmod):
    # In a separate repo because not all platforms support zlib.
    stack_snapshot(
        name = "stackage-zlib",
        extra_deps = {"zlib": ["//tests:zlib"]},
        local_snapshot = "//:stackage_snapshot.yaml",
        packages = ["zlib"],
        stack_snapshot_json = "//:stackage-zlib-snapshot.json" if not is_windows else None,
        label_builder = label_builder,
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
        setup_stack = False,
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
        label_builder = label_builder,
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

    stack_snapshot(
        name = "stackage-pinning-test",
        setup_stack = False,
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
        label_builder = label_builder,
    )

    stack_snapshot(
        name = "stackage",
        setup_stack = False,
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
        label_builder = label_builder,
    )

    if is_linux:
        stack_snapshot(
            name = "stackage_asterius",
            setup_stack = False,
            local_snapshot = "//tests/asterius/stack_toolchain_libraries:snapshot.yaml",
            packages = [
                "xhtml",
            ],
            stack_snapshot_json = "//tests/asterius/stack_toolchain_libraries:snapshot.json",
            toolchain_libraries = toolchain_libraries,
            label_builder = label_builder,
        )
    else:
        _empty_repo(
            name = "stackage_asterius",
            error_msg = "The stackage_asterius repository should only be used on linux",
        )
        _empty_repo(
            name = "stackage_asterius-unpinned",
            error_msg = "The stackage_asterius-unpinned repository should only be used on linux",
        )

def _non_module_deps_2_impl(ctx):
    repositories(bzlmod = True)

non_module_deps_2 = module_extension(
    implementation = _non_module_deps_2_impl,
)
