# This is a bit verbose, but it’s required setup
# to branch on an environment variable in a repository macro.
load(
    "@io_bazel_rules_go//go:def.bzl",
    "go_register_toolchains",
    "go_wrap_sdk",
)

def _go_wrap_sdk_unless_env_impl(rep_ctx):
    print("foo")
    if "DONT_WRAP_GO_SDK" in rep_ctx.os.environ.keys():
        # This will always download the latest go version
        print("bla")
        go_register_toolchains()
    else:
        # uses our provided go target in the toolchain
        print("wrapping")
        go_wrap_sdk(
            name = rep_ctx.name,
            root_file = rep_ctx.root_file,
        )

_go_wrap_sdk_unless_env = repository_rule(
    implementation = _go_wrap_sdk_unless_env_impl,
    environ = ["DONT_WRAP_GO_SDK"],
    attrs = {
        "root_file": attr.label()
    }
)

def go_wrap_sdk_unless_env(name, **kwargs):
    """Same as go_wrap_sdk, but checks whether the environment
    variable `DONT_WRAP_GO_SDK` is set, otherwise uses the default
    toolchain provided by rules_go."""
    _go_wrap_sdk_unless_env(name = name, **kwargs)
    print("hi")

