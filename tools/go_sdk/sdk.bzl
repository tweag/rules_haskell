def _gen_imports_impl(repository_ctx):
    repository_ctx.file("BUILD", "")

    is_windows = repository_ctx.os.name.startswith("windows")
    bzl_file_content = """
load(
    "@io_bazel_rules_go//go:def.bzl",
    "go_register_toolchains",
    "go_rules_dependencies",
)

def load_go_sdk():
    go_rules_dependencies()
    go_register_toolchains({go_version})
    """.format(
        go_version = "" if is_windows else 'go_version = "host"',
    )

    repository_ctx.file("imports.bzl", bzl_file_content)

_gen_imports = repository_rule(
    implementation = _gen_imports_impl,
    attrs = dict(),
)

def gen_imports(name):
    _gen_imports(
        name = name,
    )
