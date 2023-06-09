""" External repositories for the CI that need to be shared between WORKSPACE and MODULE.bazel files """

load("@rules_haskell//tools:os_info.bzl", "os_info")
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@rules_haskell//tools:repositories.bzl", "rules_haskell_worker_dependencies")
load(
    "@rules_nixpkgs_core//:nixpkgs.bzl",
    "nixpkgs_local_repository",
    "nixpkgs_package",
)
load("@rules_nixpkgs_python//:python.bzl", "nixpkgs_python_configure")
load("@rules_nixpkgs_go//:go.bzl", "nixpkgs_go_configure")
load("@rules_nixpkgs_cc//:cc.bzl", "nixpkgs_cc_configure")
load(
    "@rules_haskell//haskell:nixpkgs.bzl",
    "haskell_register_ghc_nixpkgs",
)
load(
    "@rules_haskell//haskell:ghc_bindist.bzl",
    "haskell_register_ghc_bindists",
)
load(
    "@rules_haskell//docs/pandoc:pandoc.bzl",
    "import_pandoc_bindists",
    "nixpkgs_pandoc_configure",
)
load(
    "@rules_haskell//:constants.bzl",
    "test_ghc_version",
)

# Replaces local_repository in bzlmod
# See https://groups.google.com/g/bazel-discuss/c/xpsg3mWQPZg
def _starlarkified_local_repository_impl(repository_ctx):
    relative_path = repository_ctx.attr.path
    workspace_root = repository_ctx.path(Label("@//:MODULE.bazel")).dirname
    absolute_path = workspace_root
    for segment in relative_path.split("/"):
        absolute_path = absolute_path.get_child(segment)
    repository_ctx.symlink(absolute_path, ".")

starlarkified_local_repository = repository_rule(
    implementation = _starlarkified_local_repository_impl,
    attrs = {
        "path": attr.string(mandatory = True),
    },
)

def repositories(*, bzlmod):
    # Some helpers for platform-dependent configuration
    os_info(name = "os_info")

    # For persistent worker (tools/worker)
    rules_haskell_worker_dependencies()

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

    starlarkified_local_repository(
        name = "tutorial",
        path = "tutorial",
    )

    starlarkified_local_repository(
        name = "examples",
        path = "examples",
    )

    starlarkified_local_repository(
        name = "examples-arm",
        path = "examples/arm",
    )

    # no modules are provided at the moment for buildifier
    http_archive(
        name = "com_github_bazelbuild_buildtools",
        sha256 = "614c84128ddb86aab4e1f25ba2e027d32fd5c6da302ae30685b9d7973b13da1b",
        strip_prefix = "buildtools-4.2.3",
        urls = ["https://github.com/bazelbuild/buildtools/archive/4.2.3.tar.gz"],
    )

    nixpkgs_local_repository(
        name = "nixpkgs_default",
        nix_file = "//nixpkgs:default.nix",
    )

    haskell_register_ghc_nixpkgs(
        attribute_path = "",
        nix_file_content = """with import <nixpkgs> {}; haskell.packages.ghc925.ghc""",
        repository = "@nixpkgs_default",
        version = test_ghc_version,
        register = not bzlmod,
    )

    haskell_register_ghc_bindists(
        register = not bzlmod,
    )

    nixpkgs_python_configure(
        repository = "@nixpkgs_default",
        register = not bzlmod,
    )

    nixpkgs_go_configure(
        sdk_name = "nixpkgs_go_sdk",
        repository = "@nixpkgs_default",
        register = not bzlmod,
        rules_go_repo_name = "io_bazel_rules_go",
    )

    nixpkgs_cc_configure(
        # Don't override the default cc toolchain needed for bindist mode.
        name = "nixpkgs_config_cc",
        repository = "@nixpkgs_default",
        register = not bzlmod,
    )

    nixpkgs_package(
        name = "zip",
        attribute_path = "zip",
        repository = "@nixpkgs_default",
    )

    nixpkgs_package(
        name = "graphviz",
        attribute_path = "graphviz",
        repository = "@nixpkgs_default",
    )

    nixpkgs_package(
        name = "sphinx",
        attribute_path = "python39Packages.sphinx",
        repository = "@nixpkgs_default",
    )

    nixpkgs_pandoc_configure(repository = "@nixpkgs_default")

    import_pandoc_bindists()

def _non_module_dev_deps_impl(_ctx):
    repositories(bzlmod = True)

non_module_dev_deps = module_extension(
    implementation = _non_module_dev_deps_impl,
)
