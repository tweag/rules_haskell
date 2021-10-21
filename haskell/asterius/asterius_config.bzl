# asterius equivalents of tools from the haskell toolchain.
ASTERIUS_BINARIES = {
    "ghc": "ahc",
    "ghc-pkg": "ahc-pkg",
}

def asterius_tools_config(host_target_cc_toolchain, posix_toolchain, node_toolchain, toolchain_bindir):
    """ Tools, PATH directories and config specific to asterius. """
    return struct(
        # Asterius needs node in the path to evaluate template
        # haskell. And the asterius bundle depends on the posix toolchain
        # because it containts wrapper scripts.
        path_for_run_ghc = ":".join(
            posix_toolchain.paths +
            [f.dirname for f in node_toolchain.nodeinfo.tool_files],
        ),
        tools_for_run_ghc = node_toolchain.nodeinfo.tool_files,
        tools_for_ghc_pkg = toolchain_bindir,

        # Asterius does not behave as other ghc cross compilers yet
        # and relies on the host cc toolchain.
        maybe_exec_target_cc_toolchain = host_target_cc_toolchain,
        supports_haddock = False,
    )
