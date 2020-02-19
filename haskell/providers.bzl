"""Providers exposed by the Haskell rules."""

HaskellInfo = provider(
    doc = "Common information about build process: dependencies, etc.",
    fields = {
        "package_databases": "Depset of package cache files.",
        "version_macros": "Depset of version macro files.",
        "import_dirs": "Import hierarchy roots.",
        "source_files": "Depset of files that contain Haskell modules.",
        "extra_source_files": "Depset of non-Haskell source files.",
        "static_libraries": "Ordered collection of compiled library archives.",
        "dynamic_libraries": "Depset of dynamic libraries.",
        "interface_dirs": "Depset of interface dirs belonging to the packages.",
        "compile_flags": "Arguments that were used to compile the code.",
    },
)

HaskellLibraryInfo = provider(
    doc = "Library-specific information.",
    fields = {
        "package_id": "Workspace unique package identifier.",
        "version": "Package version.",
        "exports": "List of other `HaskellLibraryInfo` that this package reexports",
    },
)

def all_package_ids(lib_info):
    return [lib_info.package_id] + [sublib_info.package_id for sublib_info in lib_info.exports]

# XXX: Does this belong here?
def all_dependencies_package_ids(deps):
    package_ids = []
    for dep in deps:
        if HaskellLibraryInfo in dep:
            package_ids.extend(all_package_ids(dep[HaskellLibraryInfo]))
    return package_ids

HaskellToolchainLibraryInfo = provider(
    doc = "Library that was imported via haskell_toolchain_library.",
)

HaskellCoverageInfo = provider(
    doc = "Information about coverage instrumentation for Haskell files.",
    fields = {
        "coverage_data": "A list of coverage data containing which parts of Haskell source code are being tracked for code coverage.",
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
        "cc_libraries_info": "HaskellCcLibrariesInfo over the auto-generated Haskell libraries of this proto_library target and its dependencies.",
    },
)

C2hsLibraryInfo = provider(
    doc = "Information about c2hs dependencies.",
    fields = {
        "chi_file": "c2hs interface file",
        "import_dir": "Import directory containing generated Haskell source file.",
    },
)

GhcPluginInfo = provider(
    doc = "Encapsulates GHC plugin dependencies and tools",
    fields = {
        "module": "Plugin entrypoint.",
        "deps": "Plugin dependencies.",
        "args": "Plugin options.",
        "tools": "Plugin tools.",
    },
)

HaskellCcLibrariesInfo = provider(
    doc = "Collects information to CcInfo dependencies to a Haskell target.",
    fields = {
        "libraries": "dict, maps the cc_library_key of a LibraryToLink to HaskellCcLibraryInfo.",
    },
)

HaskellCcLibraryInfo = provider(
    doc = """Extends a LibraryToLink for compatibility with GHC.

        Provides symbolic links for static libraries that don't match the
        dynamic library name due to Bazel's name mangling.

        Tracks whether a library is a Haskell or C library.
    """,
    fields = {
        "static_library_link": "File or None, mangled static library if required.",
        "pic_static_library_link": "File or None, mangled PIC static library if required.",
        "is_haskell": "bool, whether the library is a Haskell library.",
    },
)
