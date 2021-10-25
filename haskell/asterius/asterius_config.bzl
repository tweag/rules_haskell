# asterius equivalents of tools from the haskell toolchain.
ASTERIUS_BINARIES = {
    "ghc": "ahc",
    "ghc-pkg": "ahc-pkg",
}

def asterius_tools_config(exec_cc_toolchain, posix_toolchain, node_toolchain, tools_for_ghc_pkg):
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
        tools_for_ghc_pkg = tools_for_ghc_pkg,

        # Asterius does not behave as other ghc cross compilers yet
        # and relies on the cc toolchain targeting the exec platform.
        maybe_exec_cc_toolchain = exec_cc_toolchain,
        supports_haddock = False,
    )
