# Since this module depends on rules_nodejs, documentation using stardoc does not seem to be possible at the moment because of https://github.com/bazelbuild/rules_nodejs/issues/2874.

load("@bazel_tools//tools/build_defs/repo:utils.bzl", "maybe")
load(
    "@build_bazel_rules_nodejs//:index.bzl",
    "node_repositories",
    "yarn_install",
)

DEFAULT_NIXPKGS_NODEJS = "nixpkgs_nodejs"
DEFAULT_PACKAGE_JSON = "@rules_haskell//haskell/asterius:package.json"
DEFAULT_YARN_LOCK = "@rules_haskell//haskell/asterius:yarn.lock"
YARN_INSTALL_NAME = "npm_rules_haskell"

def _nixpkgs_nodejs(name, nix_repository, nixpkgs_package_rule):
    maybe(
        nixpkgs_package_rule,
        name = name,
        build_file_content = 'exports_files(glob(["{}/**"]))'.format(name),
        # XXX Indirection derivation to make all of NodeJS rooted in
        # a single directory. We shouldn't need this, but it's
        # a workaround for
        # https://github.com/bazelbuild/bazel/issues/2927.
        nix_file_content = """
        with import <nixpkgs> {{ config = {{}}; overlays = []; }};
        runCommand "nodejs-rules_haskell" {{ buildInputs = [ nodejs ]; }} ''
        mkdir -p $out/{name}
        cd $out/{name}
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

def _yarn(name, package_json, yarn_lock):
    maybe(
        yarn_install,
        name = name,
        package_json = package_json,
        yarn_lock = yarn_lock,
        symlink_node_modules = False,
        use_global_yarn_cache = False,
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
    _yarn(YARN_INSTALL_NAME, package_json, yarn_lock)
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
      nixpkgs_nodejs: The name for the nodejs that will be installed with `nixpkgs_package`.
    """
    _nixpkgs_nodejs(nixpkgs_nodejs, nix_repository, nixpkgs_package_rule)
    node_repositories(
        vendored_node = "@{}".format(nixpkgs_nodejs),
        preserve_symlinks = False,
    )
    _yarn(YARN_INSTALL_NAME, package_json, yarn_lock)

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
