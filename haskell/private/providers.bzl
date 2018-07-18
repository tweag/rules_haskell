DefaultCompileInfo = provider(
  doc = "Default compilation files and configuration.",
  fields = {
    "args": "Default argument list.",
    "ghc_args": "Arguments that were used to compile the package.",
    "inputs": "Default inputs.",
    "outputs": "Default outputs.",
    "objects_dir": "Object files directory.",
    "interfaces_dir": "Interface files directory.",
    "object_files": "Object files.",
    "object_dyn_files": "Dynamic object files.",
    "interface_files": "Interface files.",
    "modules": "Set of all module names.",
    "header_files": "Set of header files.",
    "boot_files": "Set of boot files.",
    "source_files": "Set of files that contain Haskell modules.",
    "extra_source_files": "A depset of non-Haskell source files.",
    "import_dirs": "Import hierarchy roots.",
    "env": "Default env vars.",
  },
)

HaskellBuildInfo = provider(
  doc = "Common information about build process: dependencies, etc.",
  fields = {
    "package_ids": "Set of all package ids of direct (non-prebuilt) dependencies.",
    "package_confs": "Set of package .conf files.",
    "package_caches": "Set of package cache files.",
    "static_libraries": "Ordered collection of compiled library archives.",
    "static_libraries_prof": "Ordered collection of static libraries with profiling.",
    "dynamic_libraries": "Set of dynamic libraries.",
    "interface_files": "Set of interface files belonging to the packages.",
    "prebuilt_dependencies": "Transitive collection of names of wired-in Haskell dependencies.",
    "external_libraries": "Set of dynamic shared libraries needed for linking.",
    "direct_prebuilt_deps": "Set of direct prebuilt dependencies.",
  },
)

HaskellLibraryInfo = provider(
  doc = "Library-specific information.",
  fields = {
    "package_id": "Package id, usually of the form name-version.",
    "version": "Package version.",
    "import_dirs": "Import hierarchy roots.",
    "exposed_modules": "Set of exposed module names.",
    "other_modules": "Set of non-public module names.",
    "header_files": "Set of header files.",
    "boot_files": "Set of boot files.",
    "source_files": "Set of files that contain Haskell modules.",
    "extra_source_files": "A depset of non-Haskell source files.",
    "ghc_args": "Arguments that were used to compile the package.",
  },
)

HaskellBinaryInfo = provider(
  doc = "Binary-specific information.",
  fields = {
    "import_dirs": "Import hierarchy roots.",
    "source_files": "Set of source files.",
    "modules": "Set of module names.",
    "binary": "File, compiled binary.",
    "header_files": "Set of header files.",
    "ghc_args": "Arguments that were used to compile the binary.",
  },
)

HaskellPrebuiltPackageInfo = provider(
  doc = "Information about a prebuilt GHC package.",
  fields = {
    "package": "Package name",
  },
)

HaddockInfo = provider(
  doc = "Haddock information.",
  fields = {
    "package_id": "Package id, usually of the form name-version.",
    "transitive_html": "Dictionary from package id to html dirs.",
    "transitive_haddocks": "Dictionary from package id to Haddock files.",
  }
)

HaskellLintInfo = provider(
  doc = "Provider that collects files produced by linters",
  fields = {
    "outputs": "Set of linter log files."
  }
)

HaskellProtobufInfo = provider(
  doc = "Provider that wraps providers of auto-generated Haskell libraries",
  fields = {
    "files": "files",
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
""",
    "include_directories": """
Returns the list of include directories used to compile this target.
""",
  },
)
