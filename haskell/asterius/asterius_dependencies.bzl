# Since this module depends on rules_nodejs, documentation using stardoc does not seem to be possible at the moment because of https://github.com/bazelbuild/rules_nodejs/issues/2874.

load("@bazel_tools//tools/build_defs/repo:utils.bzl", "maybe")
load(
    "@build_bazel_rules_nodejs//:index.bzl",
    "node_repositories",
    "yarn_install",
)
load("@rules_nodejs//nodejs/private:os_name.bzl", "os_name")

DEFAULT_NIXPKGS_NODEJS = "nixpkgs_nodejs"
DEFAULT_PACKAGE_JSON = "@rules_haskell//haskell/asterius:package.json"
DEFAULT_YARN_LOCK = "@rules_haskell//haskell/asterius:yarn.lock"
YARN_INSTALL_NAME = "yarn_rules_haskell"

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
        runCommand "nodejs-rules_haskell" {{ buildInputs = [ nodejs-16_x ]; }} ''
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

def _yarn_install(name, package_json, yarn_lock, yarn = None, node_repository = None):
    maybe(
        yarn_install,
        name = name,
        yarn = yarn,
        package_json = package_json,
        yarn_lock = yarn_lock,
        symlink_node_modules = False,
        use_global_yarn_cache = False,
        node_repository = node_repository,
    )

def _nixpkgs_yarn(name, nix_repository, nixpkgs_package_rule):
    maybe(
        nixpkgs_package_rule,
        name = name,
        build_file_content = 'exports_files(glob(["{}/**"]))'.format(name),
        attribute_path = "yarn",
        repository = nix_repository,
        fail_not_supported = False,
    )

def asterius_dependencies_bindist(
        package_json = DEFAULT_PACKAGE_JSON,
        yarn_lock = DEFAULT_YARN_LOCK):
    """ Install asterius dependencies for bindists.

    Args:
      package_json: The package.json file that will be passed to [yarn_install](https://bazelbuild.github.io/rules_nodejs/Built-ins.html#yarn_install)
      yarn_lock: The yarn.lock file that will be passed to [yarn_install](https://bazelbuild.github.io/rules_nodejs/Built-ins.html#yarn_install)
    """
    node_repositories(preserve_symlinks = False)
    _yarn_install(YARN_INSTALL_NAME, package_json, yarn_lock)
    _ahc_target_build_setting(name = "rules_haskell_asterius_build_setting")
    _declare_webpack(
        name = "rules_haskell_asterius_webpack",
    )

def asterius_dependencies_nix(
        nix_repository,
        nixpkgs_package_rule,
        package_json = DEFAULT_PACKAGE_JSON,
        yarn_lock = DEFAULT_YARN_LOCK,
        nixpkgs_nodejs = DEFAULT_NIXPKGS_NODEJS):
    """Install asterius dependencies based on nix.

    Args:
      nix_repository: The nix repository from which we try to install node.
      nixpkgs_package_rule: The `nixpkgs_package` rule from `rules_nixpkgs`.
      package_json: The package.json file that will be passed to [yarn_install](https://bazelbuild.github.io/rules_nodejs/Built-ins.html#yarn_install)
      yarn_lock: The yarn.lock file that will be passed to [yarn_install](https://bazelbuild.github.io/rules_nodejs/Built-ins.html#yarn_install)
      nixpkgs_nodejs: The prefix for the for the nodejs repositories that will be installed with `nixpkgs_package`.
    """

    # rules_nodejs declares nodejs installations for each known
    # platform and the yarn_install repository rules expects the name
    # to end with the os_name.  Because of this we declare the nix
    # installation multiple times but only the one corresponding to
    # the current os will be used.
    # The following file contains the platforms suported by rules_nodejs via bindists.
    # https://github.com/bazelbuild/rules_nodejs/blob/43159a5b5b2c5a7027b7d062e9d75025c1e65acd/nodejs/private/toolchains_repo.bzl
    rules_nodejs_platforms = [
        "darwin_amd64",
        "darwin_arm64",
        "linux_amd64",
        "linux_arm64",
        "linux_s390x",
        "linux_ppc64le",
    ]

    for os in rules_nodejs_platforms:
        _nixpkgs_nodejs("{}_{}".format(nixpkgs_nodejs, os), nix_repository, nixpkgs_package_rule)

    _nixpkgs_yarn("rules_haskell_nix_yarn", nix_repository, nixpkgs_package_rule)

    _yarn_install(
        YARN_INSTALL_NAME,
        package_json,
        yarn_lock,
        yarn = "@rules_haskell_nix_yarn//:bin/yarn",
        node_repository = nixpkgs_nodejs,
    )

    _declare_nix_node_toolchain(
        name = "rules_haskell_nix_node_toolchain",
        nixpkgs_nodejs = nixpkgs_nodejs,
    )
    native.register_toolchains("@rules_haskell_nix_node_toolchain//:node_nixpkgs_toolchain")

    _ahc_target_build_setting(name = "rules_haskell_asterius_build_setting")
    _declare_webpack(
        name = "rules_haskell_asterius_webpack",
    )

def asterius_dependencies_custom(webpack_rule):
    """Setup asterius to use an existing nodejs environment.

    Args:
      webpack_rule: The label of a webpack rule declared with rules_nodejs.
        For instance as such:
        ```
        load("@npm//webpack-cli:index.bzl", webpack = "webpack_cli")
        webpack(
            name = "webpack",
            visibility = ["//visibility:public"],
        )
        ```
    """
    _ahc_target_build_setting(name = "rules_haskell_asterius_build_setting")
    _declare_webpack(
        name = "rules_haskell_asterius_webpack",
        webpack_rule = webpack_rule,
    )

def _declare_nix_node_toolchain_impl(repository_ctx):
    # The yarn install repository rule expects the repository
    # containing the node installation to end with the os_name.
    name_of_os = os_name(repository_ctx)
    build_content = """
load("@rules_nodejs//nodejs:toolchain.bzl", "node_toolchain")

node_toolchain(
    name = "node_nixpkgs",
    target_tool = "@{nixpkgs_nodejs}_{name_of_os}//:bin/node",
)

toolchain(
    name = "node_nixpkgs_toolchain",
    exec_compatible_with = [
        "@io_tweag_rules_nixpkgs//nixpkgs/constraints:support_nix",
    ],
    target_compatible_with = [
        "@io_tweag_rules_nixpkgs//nixpkgs/constraints:support_nix",
    ],
    toolchain = ":node_nixpkgs",
    toolchain_type = "@rules_nodejs//nodejs:toolchain_type",
)

        """.format(
        name_of_os = name_of_os,
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
    if repository_ctx.attr.webpack_rule:
        build_content = """
alias(
    name="webpack",
    actual="{}",
    visibility = ["//visibility:public"],
)
        """.format(repository_ctx.attr.webpack_rule)
    else:
        build_content = """
load("@{}//webpack-cli:index.bzl", webpack = "webpack_cli")
webpack(
    name = "webpack",
    visibility = ["//visibility:public"],
)
        """.format(YARN_INSTALL_NAME)

    repository_ctx.file(
        "BUILD",
        content = build_content,
        executable = False,
    )

_declare_webpack = repository_rule(
    implementation = _declare_webpack_impl,
    attrs = {
        "webpack_rule": attr.label(
            default = None,
            doc = "An optional webpack_rule to override the default one",
        ),
    },
    doc = "This repository rule declares the `webpack` rule used by asterius.",
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
