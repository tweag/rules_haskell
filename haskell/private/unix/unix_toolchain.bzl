load("@bazel_skylib//lib:paths.bzl", "paths")
load(
    "@rules_haskell//haskell/private/unix:unix_commands.bzl",
    "unix_commands",
)

UnixCommandsInfo = provider(
    doc = "Collection of Unix commands",
    fields = {
        cmd: "Absolute path to the {} command or None.".format(cmd)
        for cmd in unix_commands
    },
)

def _unix_toolchain_impl(ctx):
    commands = {}
    for cmd in unix_commands:
        cmd_path = getattr(ctx.attr, cmd, None)
        if not cmd_path:
            cmd_path = None
        commands[cmd] = cmd_path
    cmd_paths = {
        paths.dirname(cmd_path): None
        for cmd_path in commands.values()
        if cmd_path
    }.keys()
    return [platform_common.ToolchainInfo(
        commands = commands,
        paths = cmd_paths,
    )]

unix_toolchain = rule(
    attrs = {
        cmd: attr.string(
            doc = "Absolute path to the {} command.".format(cmd),
            mandatory = False,
        )
        for cmd in unix_commands
    },
    implementation = _unix_toolchain_impl,
)
"""A toolchain capturing standard Unix shell tools.

These tools are required by the Cabal build system and ./configure style Cabal
packages.

Use `unix_nixpkgs_toolchain` or `unix_configure` to create an instance of this
toolchain.
"""
