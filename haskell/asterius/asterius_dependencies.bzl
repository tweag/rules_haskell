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

def asterius_dependencies_nix(
        nix_repository,
        nixpkgs_package_rule,
        package_json = DEFAULT_PACKAGE_JSON,
        yarn_lock = DEFAULT_YARN_LOCK,
        nixpkgs_nodejs = DEFAULT_NIXPKGS_NODEJS):
    """
    Install asterius dependencies based on nix.

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
