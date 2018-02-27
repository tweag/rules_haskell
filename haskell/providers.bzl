HaskellPackageInfo = provider(
  doc = "Package information exposed by Haskell libraries.",
  fields = {
    "name": "Package name, usually of the form name-version.",
    "names": "All package names of transitive dependencies. Includes own name.",
    "confs": "Package conf files.",
    "caches": "Package cache files.",
    "static_libraries": "Compiled library archives.",
    "dynamic_libraries": "Dynamic libraries.",
    "interface_files": "Interface files belonging to the packages.",
    "prebuilt_dependencies": "Transitive collection of all wired-in Haskell dependencies.",
    "external_libraries": "Dynamic shared libraries needed for linking. List of .so files.",
    "import_dirs": "A set of import directories (useful with GHCi for example)",
    "exposed_modules": "A set of exposed module names.",
    "hidden_modules": "A set of hidden module names.",
    "haddock_ghc_args": "Arguments that were used to compile the package, suitable for Haddock."
  }
)

HaddockInfo = provider(
  doc = "Haddock information.",
  fields = {
    "outputs": "All interesting outputs produced by Haddock.",
    "interface_file": "Haddock interface file.",
    "doc_dir": "Directory where all the documentation files live.",
  }
)

# XXX this provider shouldn't be necessary. But since Skylark rules
# can neither return CcSkylarkApiProvider nor properly test for its
# existence in a dependency, we're forced to introduce this hack for
# now. See https://github.com/bazelbuild/bazel/issues/4370.
CcSkylarkApiProviderHacked = provider(
  doc = "Skylark emulation of CcSkylarkApiProvider. Temporary hack.",
  fields = {
    "transitive_headers": """

Returns a depset of headers that have been declared in the src or
headers attribute(possibly empty but never None).
"""
  },
)
