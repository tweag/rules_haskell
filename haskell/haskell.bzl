"""Core Haskell rules (deprecated, use defs.bzl instead)"""

load(
    ":defs.bzl",
    _ghc_plugin = "ghc_plugin",
    _haskell_binary = "haskell_binary",
    _haskell_doc = "haskell_doc",
    _haskell_doc_aspect = "haskell_doc_aspect",
    _haskell_import = "haskell_import",
    _haskell_library = "haskell_library",
    _haskell_register_toolchains = "haskell_register_toolchains",
    _haskell_repl = "haskell_repl",
    _haskell_repl_aspect = "haskell_repl_aspect",
    _haskell_test = "haskell_test",
    _haskell_toolchain = "haskell_toolchain",
    _haskell_toolchain_libraries = "haskell_toolchain_libraries",
    _haskell_toolchain_library = "haskell_toolchain_library",
)
load(
    ":doctest.bzl",
    _haskell_doctest = "haskell_doctest",
    _haskell_doctest_toolchain = "haskell_doctest_toolchain",
)
load(
    ":protobuf.bzl",
    _haskell_proto_library = "haskell_proto_library",
    _haskell_proto_toolchain = "haskell_proto_toolchain",
)
load(
    ":ghc_bindist.bzl",
    _ghc_bindist = "ghc_bindist",
    _haskell_register_ghc_bindists = "haskell_register_ghc_bindists",
)

haskell_test = _haskell_test
haskell_binary = _haskell_binary
haskell_library = _haskell_library
haskell_import = _haskell_import
haskell_toolchain_libraries = _haskell_toolchain_libraries
haskell_toolchain_library = _haskell_toolchain_library
haskell_doc = _haskell_doc
haskell_doc_aspect = _haskell_doc_aspect
haskell_register_toolchains = _haskell_register_toolchains
haskell_repl = _haskell_repl
haskell_repl_aspect = _haskell_repl_aspect
haskell_toolchain = _haskell_toolchain
ghc_plugin = _ghc_plugin

# The following reexports are not in defs.bzl.

haskell_doctest = _haskell_doctest
haskell_doctest_toolchain = _haskell_doctest_toolchain

haskell_proto_library = _haskell_proto_library
haskell_proto_toolchain = _haskell_proto_toolchain

ghc_bindist = _ghc_bindist
haskell_register_ghc_bindists = _haskell_register_ghc_bindists
