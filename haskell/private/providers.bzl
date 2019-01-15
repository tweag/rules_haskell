DefaultCompileInfo = provider(
    doc = "Default compilation files and configuration.",
    fields = {
        "args": "Default argument list.",
        "ghc_args": "Arguments that were used to compile the package.",
        "inputs": "Default inputs.",
        "outputs": "Default outputs.",
        "objects_dir": "Object files directory.",
        "interfaces_dir": "Interface files directory.",
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
        "interface_dirs": "Set of interface dirs belonging to the packages.",
        "prebuilt_dependencies": "Transitive collection of info of wired-in Haskell dependencies.",
        "external_libraries": "Set of dynamic shared libraries needed for linking. " +
                              "Each entry is a struct(lib, mangled_lib) " +
                              "because the Darwin linker needs the original library path, " +
                              "while the Linux linker needs the mangled path.",
        "direct_prebuilt_deps": "Set of info of direct prebuilt dependencies.",
        "extra_libraries": "The direct non-Haskell dependencies of this target. These are added to the extra-libraries field in the package's database.",
    },
)

def external_libraries_get_mangled(ext_lib):
    """Just a dumb helper because skylark doesn’t do lambdas."""
    return ext_lib.mangled_lib

HaskellLibraryInfo = provider(
    doc = "Library-specific information.",
    fields = {
        "package_id": "Workspace unique package identifier.",
        "version": "Package version.",
        "import_dirs": "Import hierarchy roots.",
        "header_files": "Set of header files.",
        "boot_files": "Set of boot files.",
        "source_files": "Set of files that contain Haskell modules.",
        "extra_source_files": "A depset of non-Haskell source files.",
        "ghc_args": "Arguments that were used to compile the package.",
        "exposed_modules_file": "File containing a list of exposed module names.",
    },
)

HaskellBinaryInfo = provider(
    doc = "Binary-specific information.",
    fields = {
        "import_dirs": "Import hierarchy roots.",
        "source_files": "Set of source files.",
        "binary": "File, compiled binary.",
        "header_files": "Set of header files.",
        "ghc_args": "Arguments that were used to compile the binary.",
        "exposed_modules_file": "File containing a list of exposed module names.",
    },
)

HaskellPrebuiltPackageInfo = provider(
    doc = "Information about a prebuilt GHC package.",
    fields = {
        "package": "Package name",
        "id_file": "File containing package id",
    },
)

HaddockInfo = provider(
    doc = "Haddock information.",
    fields = {
        "package_id": "Package id, usually of the form name-version.",
        "transitive_html": "Dictionary from package id to html dirs.",
        "transitive_haddocks": "Dictionary from package id to Haddock files.",
    },
)

HaskellLintInfo = provider(
    doc = "Provider that collects files produced by linters",
    fields = {
        "outputs": "Set of linter log files.",
    },
)

HaskellProtobufInfo = provider(
    doc = "Provider that wraps providers of auto-generated Haskell libraries",
    fields = {
        "files": "files",
    },
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

C2hsLibraryInfo = provider(
    doc = "Information about c2hs dependencies.",
    fields = {
        "chi_file": "c2hs interface file",
        "import_dir": "Import directory containing generated Haskell source file.",
    },
)
