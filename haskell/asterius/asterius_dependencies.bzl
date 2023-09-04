# Since this module depends on rules_nodejs, documentation using stardoc does not seem to be possible at the moment because of https://github.com/bazelbuild/rules_nodejs/issues/2874.

load("@aspect_rules_js//npm:npm_import.bzl", "npm_translate_lock")
load("@bazel_tools//tools/build_defs/repo:utils.bzl", "maybe")
load("@rules_nodejs//nodejs:repositories.bzl", "nodejs_register_toolchains")

DEFAULT_NIXPKGS_NODEJS = "nixpkgs_nodejs"
DEFAULT_PNPM_LOCK = "@rules_haskell//haskell/asterius/npm:pnpm-lock.yaml"
DEFAULT_WEBPACK_CLI_PACKAGE_JSON_BZL = "@rules_haskell_npm//haskell/asterius/npm:webpack-cli/package_json.bzl"

def _nixpkgs_nodejs(name, nix_repository, nixpkgs_package_rule):
    maybe(
        nixpkgs_package_rule,
        name = name,
        build_file_content = 'exports_files(glob(["**"]))',
        # XXX Indirection derivation to make all of NodeJS rooted in
        # a single directory. We shouldn't need this, but it's
        # a workaround for
        # https://github.com/bazelbuild/bazel/issues/2927.
        nix_file_content = """
        with import <nixpkgs> {{ config = {{}}; overlays = []; }};
        runCommand "nodejs-rules_haskell" {{ buildInputs = [ nodejs-18_x ]; }} ''
        mkdir -p $out
        cd $out
        for i in ${{nodejs}}/*; do ln -s $i; done
        ''
        """.format(name = name),
        nixopts = [
            "--option",
            "sandbox",
            "false",
        ],
        repository = nix_repository,
        fail_not_supported = False,
    )

def asterius_dependencies_bindist(
        register = True):
    """ Install asterius dependencies for bindists.

A `rules_haskell_npm` repository is created that needs to be used as follow in the WORKSPACE file:

```
load("@rules_haskell_npm//:repositories.bzl", "npm_repositories")
npm_repositories()
```

    Args:
      register: Whether to register the asterius toolchain (must be set to False if bzlmod is activated)
    """
    _ahc_target_build_setting(name = "rules_haskell_asterius_build_setting")
    if register:
        nodejs_register_toolchains(
            name = "nodejs",
            node_version = "16.19.0",
        )
        npm_translate_lock(
            name = "rules_haskell_npm",
            pnpm_lock = DEFAULT_PNPM_LOCK,
            verify_node_modules_ignored = "@rules_haskell//:.bazelignore",
            link_workspace = "rules_haskell",
        )
    _declare_webpack(
        name = "rules_haskell_asterius_webpack",
    )

def asterius_dependencies_nix(
        nix_repository,
        nixpkgs_package_rule,
        nixpkgs_nodejs = DEFAULT_NIXPKGS_NODEJS,
        register = True):
    """Install asterius dependencies based on nix.

A `rules_haskell_npm` repository is created that needs to be used as follow in the WORKSPACE file:

```
load("@rules_haskell_npm//:repositories.bzl", "npm_repositories")
npm_repositories()
```

    Args:
      nix_repository: The nix repository from which we try to install node.
      nixpkgs_package_rule: The `nixpkgs_package` rule from `rules_nixpkgs`.
      nixpkgs_nodejs: The prefix for the for the nodejs repositories that will be installed with `nixpkgs_package`.
      register: Whether to register the asterius toolchain (must be set to False if bzlmod is activated)
    """

    _nixpkgs_nodejs(nixpkgs_nodejs, nix_repository, nixpkgs_package_rule)

    _declare_nix_node_toolchain(
        name = "rules_haskell_nix_node_toolchain",
        nixpkgs_nodejs = nixpkgs_nodejs,
    )
    if register:
        native.register_toolchains("@rules_haskell_nix_node_toolchain//:node_nixpkgs_toolchain")
        npm_translate_lock(
            name = "rules_haskell_npm",
            pnpm_lock = DEFAULT_PNPM_LOCK,
            verify_node_modules_ignored = "@rules_haskell//:.bazelignore",
            link_workspace = "rules_haskell",
        )
    _ahc_target_build_setting(name = "rules_haskell_asterius_build_setting")

    _declare_webpack(
        name = "rules_haskell_asterius_webpack",
    )

def asterius_dependencies_custom(webpack_cli_package_json_bzl):
    """Setup asterius to use an existing rules_js environment.

    Args:
      webpack_cli_package_json_bzl: The label of a webpack-cli/package_json.bzl
       file generated by rules_js. For instance via `npm_translate_lock`.
    """
    _ahc_target_build_setting(name = "rules_haskell_asterius_build_setting")
    _declare_webpack(
        name = "rules_haskell_asterius_webpack",
        webpack_cli_package_json_bzl = webpack_cli_package_json_bzl,
    )

def _declare_nix_node_toolchain_impl(repository_ctx):
    build_content = """
load("@rules_nodejs//nodejs:toolchain.bzl", "node_toolchain")

node_toolchain(
    name = "node_nixpkgs",
    target_tool = "@{nixpkgs_nodejs}//:bin/node",
)

toolchain(
    name = "node_nixpkgs_toolchain",
    exec_compatible_with = [
        "@rules_nixpkgs_core//constraints:support_nix"
    ],
    target_compatible_with = [
        "@rules_nixpkgs_core//constraints:support_nix"
    ],
    toolchain = ":node_nixpkgs",
    toolchain_type = "@rules_nodejs//nodejs:toolchain_type",
)

        """.format(
        nixpkgs_nodejs = repository_ctx.attr.nixpkgs_nodejs,
    )

    repository_ctx.file(
        "BUILD",
        content = build_content,
        executable = False,
    )

_declare_nix_node_toolchain = repository_rule(
    implementation = _declare_nix_node_toolchain_impl,
    attrs = {
        "nixpkgs_nodejs": attr.string(
            doc = "The prefix of the nixpkgs nodejs installation",
        ),
    },
    doc = "This repository rule declares the nix node toolchain.",
)

def _declare_webpack_impl(repository_ctx):
    repository_ctx.file(
        "BUILD",
        content = "",
        executable = False,
    )
    package_json_bzl = repository_ctx.attr.webpack_cli_package_json_bzl or DEFAULT_WEBPACK_CLI_PACKAGE_JSON_BZL
    repository_ctx.file(
        "package_json.bzl",
        content = """
load("{}", _bin = "bin")
bin = _bin
""".format(package_json_bzl),
        executable = False,
    )

_declare_webpack = repository_rule(
    implementation = _declare_webpack_impl,
    attrs = {
        "webpack_cli_package_json_bzl": attr.label(
            default = None,
            doc = "An optional webpack-cli/package_json.bzl target (from rules_js) to override the default one",
        ),
    },
    doc = "This repository defines the `webpack` used for asterius tests.",
)

def _ahc_target_build_setting_impl(repository_ctx):
    repository_ctx.file(
        "BUILD",
        content = """
# This build settings controls whether the ahc_dist rule targets node
# or the browser.
# It is defined outside of the rules_haskell repository as a
# work-around for the following issue:
# https://github.com/bazelbuild/bazel/issues/12951

load("@bazel_skylib//rules:common_settings.bzl", "bool_flag")
bool_flag(
    name = "asterius_targets_browser",
    build_setting_default = False,
    visibility = ["//visibility:public"],
)
        """,
        executable = False,
    )

_ahc_target_build_setting = repository_rule(
    implementation = _ahc_target_build_setting_impl,
    doc = "This repository rule declares the `asterius_targets_browser` build setting.",
)
