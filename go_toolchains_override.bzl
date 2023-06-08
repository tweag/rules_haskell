""" Override go toolchain used by """

def _go_toolchains_override_repo_rule_impl(rctx):
    rctx.file("BUILD", content = "")
    if rctx.which("nix-build"):
        # This toolchain override may only be selected if nix is available
        toolchain = """Label("@nixpkgs_go_sdk//:ROOT")"""
    else:
        toolchain = None
    rctx.file("go_toolchain.bzl", content = """
toolchain = {toolchain}
""".format(toolchain = toolchain))

_go_toolchains_override_repo_rule = repository_rule(
    implementation = _go_toolchains_override_repo_rule_impl,
)

def _go_toolchains_override_impl(_ctx):
    _go_toolchains_override_repo_rule(name = "rules_go_toolchain_override")

rules_go_toolchain_override = module_extension(
    implementation = _go_toolchains_override_impl,
)
