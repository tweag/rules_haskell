HaskellBuildInfo = provider(
  doc = "Common information about build process: dependencies, etc.",
  fields = {
    "package_names": "Set of all package names of transitive dependencies.",
    "package_confs": "Set of package .conf files.",
    "package_caches": "Set of package cache files.",
    "static_libraries": "Ordered collection of compiled library archives.",
    "dynamic_libraries": "Set of dynamic libraries.",
    "interface_files": "Set of interface files belonging to the packages.",
    "prebuilt_dependencies": "Transitive collection of names of wired-in Haskell dependencies.",
    "external_libraries": "Set of dynamic shared libraries needed for linking.",
  },
)

HaskellLibraryInfo = provider(
  doc = "Library-specific information.",
  fields = {
    "package_name": "Package name, usually of the form name-version.",
    "import_dir": "Import hierarchy root.",
    "exposed_modules": "Set of exposed module names.",
    "other_modules": "Set of non-public module names.",
    "haddock_args": "Arguments that were used to compile the package suitable for Haddock."
  },
)

HaskellBinaryInfo = provider(
  doc = "Binary-specific information.",
  fields = {
    "source_files": "Set of source files.",
    "modules": "Set of module names.",
  },
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
