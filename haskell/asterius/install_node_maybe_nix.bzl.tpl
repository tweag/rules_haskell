load(
     "@build_bazel_rules_nodejs//:index.bzl",
     "node_repositories",
     "yarn_install",
     )

DEFAULT_PACKAGE_JSON = "{DEFAULT_PACKAGE_JSON}"
DEFAULT_YARN_LOCK = "{DEFAULT_YARN_LOCK}"
YARN_INSTALL_NAME = "{YARN_INSTALL_NAME}"

def asterius_node_dependencies(package_json = DEFAULT_PACKAGE_JSON, yarn_lock = DEFAULT_YARN_LOCK):
    node_repositories(
        {MAYBE_VENDORED_NODE}
        preserve_symlinks = False,
    )

    yarn_install(
        name = YARN_INSTALL_NAME,
        package_json = package_json,
        yarn_lock = yarn_lock,
        symlink_node_modules = False,
        use_global_yarn_cache = False,
    )