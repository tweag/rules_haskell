load("@bazel_tools//tools/cpp:lib_cc_configure.bzl", "get_cpu_value")

_os_info_bzl_template = """
cpu_value = "{CPU_VALUE}"
is_darwin = cpu_value == "darwin" or cpu_value == "darwin_arm64"
is_darwin_arm64 = cpu_value == "darwin_arm64"
is_linux = cpu_value == "k8"
is_windows = cpu_value == "x64_windows"
nix_shell = {NIX_SHELL}
is_nix_shell = nix_shell != None
"""

def _os_info_impl(repository_ctx):
    cpu = get_cpu_value(repository_ctx)
    known_cpu_values = [
        "darwin",
        "darwin_arm64",
        "k8",
        "x64_windows",
    ]
    if cpu not in known_cpu_values:
        fail("Unknown OS type {}, expected one of {}".format(cpu, ", ".join(known_cpu_values)))
    nix_shell = repository_ctx.os.environ.get("IN_NIX_SHELL")
    os_info_substitutions = {
        "CPU_VALUE": cpu,
        "NIX_SHELL": repr(nix_shell),
    }
    repository_ctx.file(
        "os_info.bzl",
        _os_info_bzl_template.format(**os_info_substitutions),
        False,
    )
    repository_ctx.file(
        "BUILD",
        "",
        False,
    )

os_info = repository_rule(
    implementation = _os_info_impl,
    environ = ["IN_NIX_SHELL"],
    configure = True,
    local = True,
)
