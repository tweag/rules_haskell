# From the asterius/cabal/config file of asterius and ahc-cabal.hs
asterius_cabalopts = [
    "--enable-library-vanilla",
    "--disable-executable-dynamic",
    "--disable-profiling",
    "-O 2",
    "--disable-debug-info",
    "--disable-library-for-ghci",
    "--disable-split-sections",
    "--disable-split-objs",
    "--disable-executable-stripping",
    "--disable-library-stripping",
    "--disable-tests",
    "--disable-coverage",
    "--disable-benchmarks",
    "--hsc2hs-options=--cross-compile",
]

# asterius equivalents of tools from the haskell toolchain.
ASTERIUS_BINARIES = {
    "ahc": "ghc",
    "ahc-pkg": "ghc_pkg",
}

def asterius_tools_config(exec_cc_toolchain, posix_toolchain, node_toolchain, tools_for_ghc_pkg):
    """ Tools, PATH directories and config specific to asterius. """
    node_paths = [f.dirname for f in node_toolchain.nodeinfo.tool_files]
    return struct(
        # Asterius needs node in the path to evaluate template
        # haskell. And the asterius bundle depends on the posix toolchain
        # because it containts wrapper scripts.
        path_for_run_ghc = posix_toolchain.paths + node_paths,
        path_for_cabal = node_paths,
        tools_for_ghc = node_toolchain.nodeinfo.tool_files,
        tools_for_ghc_pkg = tools_for_ghc_pkg,

        # Asterius does not behave as other ghc cross compilers yet
        # and relies on the cc toolchain targeting the exec platform.
        maybe_exec_cc_toolchain = exec_cc_toolchain,
        supports_haddock = False,
    )
