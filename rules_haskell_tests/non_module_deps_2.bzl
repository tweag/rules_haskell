""" External repositories for the CI that need to be shared between WORKSPACE and MODULE.bazel files """

load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")
load("@os_info//:os_info.bzl", "is_linux", "is_windows")
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@toolchains_libraries//:toolchain_libraries.bzl", "toolchain_libraries")
load("@rules_haskell_ghc_version//:ghc_version.bzl", "GHC_VERSION")

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
        local_snapshot = "//:stackage_snapshot{}.yaml".format(
            "_" + str(GHC_VERSION) if GHC_VERSION else "",
        ),
        packages = ["zlib"],
        stack_snapshot_json = ("//:stackage-zlib-snapshot{}.json".format(
            "_" + str(GHC_VERSION) if GHC_VERSION else "",
        )) if not is_windows else None,
        label_builder = label_builder,
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
        extra_deps = {"zlib": ["//tests:zlib"], "streaming-commons": ["//tests:zlib"]},
        haddock = False,
        local_snapshot = "//:ghcide-stack-snapshot{}.yaml".format(
            "_" + str(GHC_VERSION) if GHC_VERSION else "",
        ),
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
            "hw-prim": ["@ghcide//:Cabal"],
            "hw-fingertree": ["@ghcide//:Cabal"],
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
            "regex-pcre-builtin": ["@ghcide//:Cabal"],
            "transformers-compat": ["@ghcide//:Cabal"],
            "typed-process": ["@ghcide//:Cabal"],
            "unliftio": ["@ghcide//:Cabal"],
            "unliftio-core": ["@ghcide//:Cabal"],
            "yaml": ["@ghcide//:Cabal"],
        },
        stack_snapshot_json = ("//:ghcide-snapshot{}.json".format(
            "_" + str(GHC_VERSION) if GHC_VERSION else "",
        )) if not is_windows else None,
        vendored_packages = {
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

    if GHC_VERSION and GHC_VERSION.startswith("9.4."):
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
    version = "3.8.1.0",
    visibility = ["//visibility:public"],
)
""",
            sha256 = "b697b558558f351d2704e520e7dcb1f300cd77fea5677d4b2ee71d0b965a4fe9",
            strip_prefix = "cabal-ghc-9.4-paths-module-relocatable",
            urls = ["https://github.com/tweag/cabal/archive/refs/heads/ghc-9.4-paths-module-relocatable.zip"],
        )
    else:
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
        local_snapshot = "//:stackage-pinning-test{}.yaml".format(
            "_" + str(GHC_VERSION) if GHC_VERSION else "",
        ),
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
        stack_snapshot_json = ("//:stackage-pinning-test_snapshot{}.json".format(
            "_" + str(GHC_VERSION) if GHC_VERSION else "",
        )) if not is_windows else None,
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
        # On non linux, we still need a dummy repository to call `use_repo` in the `MODULE.bazel` file.
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
