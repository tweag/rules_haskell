""" External repositories for the CI that need to be shared between WORKSPACE and MODULE.bazel files """

load("@rules_haskell//tools:os_info.bzl", "os_info")
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
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
    "@rules_haskell//docs/pandoc:pandoc.bzl",
    "import_pandoc_bindists",
    "nixpkgs_pandoc_configure",
)
load(
    "@rules_haskell//:constants.bzl",
    _default_ghc_version = "test_ghc_version",
)
load("@rules_haskell_ghc_version//:ghc_version.bzl", "GHC_VERSION")

test_ghc_version = GHC_VERSION or _default_ghc_version

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
        sha256 = "42968f9134ba2c75c03bb271bd7bb062afb7da449f9b913c96e5be4ce890030a",
        # fix runner.bash.template always returning success, format MODULE.bazel and WORKSPACE.bzlmod too
        patches = ["@rules_haskell//buildifier:buildifier_test-workspace.patch"],
        patch_args = ["-p1"],
        strip_prefix = "buildtools-6.3.3",
        urls = ["https://github.com/bazelbuild/buildtools/archive/v6.3.3.tar.gz"],
    )

    nixpkgs_local_repository(
        name = "nixpkgs_default",
        nix_file = "//nixpkgs:default.nix",
    )

    haskell_register_ghc_nixpkgs(
        attribute_path = "",
        nix_file_content = """with import <nixpkgs> {{}}; haskell.packages.ghc{version}.ghc""".format(
            version = test_ghc_version.replace(".", ""),
        ),
        repository = "@nixpkgs_default",
        version = test_ghc_version,
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
