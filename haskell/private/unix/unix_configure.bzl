load("@bazel_tools//tools/cpp:lib_cc_configure.bzl", "get_cpu_value")
load(
    "@rules_haskell//haskell/private/unix:unix_commands.bzl",
    "unix_commands",
)

def _unix_config_impl(repository_ctx):
    is_windows = repository_ctx.os.name.startswith("windows")
    commands = {}
    for cmd in unix_commands:
        if is_windows:
            cmd_path = repository_ctx.which(cmd + ".exe")
        else:
            cmd_path = repository_ctx.which(cmd)
        commands[cmd] = cmd_path
    cpu = get_cpu_value(repository_ctx)
    repository_ctx.file("BUILD.bazel", executable = False, content = """
load(
    "@rules_haskell//haskell/private/unix:unix_toolchain.bzl",
    "unix_toolchain",
)
unix_toolchain(
    name = "local_unix",
    visibility = ["//visibility:public"],
    {commands}
)
toolchain(
    name = "local_unix_toolchain",
    toolchain = ":local_unix",
    toolchain_type = "@rules_haskell//haskell/private/unix:toolchain_type",
    exec_compatible_with = [
        "@bazel_tools//platforms:x86_64",
        "@bazel_tools//platforms:{os}",
    ],
    target_compatible_with = [
        "@bazel_tools//platforms:x86_64",
        "@bazel_tools//platforms:{os}",
    ],
)
""".format(
        commands = ",\n    ".join([
            '{cmd} = "{path}"'.format(cmd = cmd, path = cmd_path)
            for (cmd, cmd_path) in commands.items()
            if cmd_path
        ]),
        os = "osx" if cpu == "darwin" else "linux",
    ))

unix_config = repository_rule(
    environ = ["PATH"],
    local = True,
    implementation = _unix_config_impl,
)

def unix_configure(name = "local_unix_config"):
    """Autodetect local Unix tools.

    Scans the environment (`$PATH`) for standard shell tools, generates a
    corresponding unix toolchain and registers the toolchain.
    """
    unix_config(name = name)
    native.register_toolchains("@{}//:local_unix_toolchain".format(name))
