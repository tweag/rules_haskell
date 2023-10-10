load("@rules_haskell//haskell:defs.bzl", "haskell_binary")
load(
    "@rules_bazel_integration_test//bazel_integration_test:defs.bzl",
    "bazel_integration_test",
    "integration_test_utils",
)
load("@cgrindel_bazel_starlib//shlib/rules:execute_binary.bzl", "execute_binary")

DEFAULT_TAGS = ["exclusive", "integration"]
DEFAULT_HASKELL_DEPS = [
    "//tests/integration_testing",
    "//tests/hackage:base",
    "@stackage//:hspec",
    "@stackage//:hspec-core",
]

def haskell_bazel_integration_test(
        name,
        srcs,
        bazel_binaries,
        workspace_path,
        args = [],
        env = {},
        deps = [],
        rule_files = [],
        **kwargs):
    kwargs["tags"] = kwargs.pop("tags", []) + DEFAULT_TAGS

    binary_name = "%s_bin" % name
    haskell_binary(
        name = binary_name,
        srcs = srcs,
        deps = deps + DEFAULT_HASKELL_DEPS,
        testonly = True,
    )

    runner_name = "%s_wrapped" % binary_name
    execute_binary(
        name = runner_name,
        binary = binary_name,
        arguments = args,
        testonly = True,
    )

    for bazel_id, bazel_binary in bazel_binaries.items():
        bazel_integration_test(
            name = "%s_%s" % (name, bazel_id),
            test_runner = runner_name,
            bazel_binary = bazel_binary,
            workspace_files = integration_test_utils.glob_workspace_files(workspace_path) + rule_files,
            workspace_path = workspace_path,
            env = dict(env, **{"BIT_BAZEL_BIN_ID": bazel_id}),
            **kwargs
        )
