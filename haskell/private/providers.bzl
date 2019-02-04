load(
    ":private/path_utils.bzl",
    "darwin_convert_to_dylibs",
    "is_shared_library",
)
load(":private/set.bzl", "set")

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

HaskellCcInfo = provider(
    doc = "Haskell cc dependency information. Part of HaskellBuildInfo.",
    fields = {
        "static_linking": """static linking mode parameters.
            A struct of
            (libraries_to_link, dynamic_libraries_for_runtime, user_link_flags).
            Libraries in libraries_to_link are struct(lib, mangled_lib)
            because the Darwin linker needs the original library path,
            while the Linux linker needs the mangled path.
            """,
        "dynamic_linking": """static linking mode parameters.
            A struct of
            (libraries_to_link, dynamic_libraries_for_runtime, user_link_flags).
            Libraries in libraries_to_link are struct(lib, mangled_lib)
            because the Darwin linker needs the original library path,
            while the Linux linker needs the mangled path.
            """,
    },
)

def empty_HaskellCcInfo():
    return HaskellCcInfo(
        static_linking = struct(
            libraries_to_link = depset(order = "topological"),
            dynamic_libraries_for_runtime = depset(order = "topological"),
            user_link_flags = depset(order = "topological"),
        ),
        dynamic_linking = struct(
            libraries_to_link = depset(order = "topological"),
            dynamic_libraries_for_runtime = depset(order = "topological"),
            user_link_flags = depset(order = "topological"),
        ),
    )

def merge_HaskellCcInfo(*args):
    return HaskellCcInfo(
        static_linking = struct(
            libraries_to_link = depset(
                order = "topological",
                transitive = [arg.static_linking.libraries_to_link for arg in args],
            ),
            dynamic_libraries_for_runtime = depset(
                order = "topological",
                transitive = [arg.static_linking.dynamic_libraries_for_runtime for arg in args],
            ),
            user_link_flags = depset(
                order = "topological",
                transitive = [arg.static_linking.user_link_flags for arg in args],
            ),
        ),
        dynamic_linking = struct(
            libraries_to_link = depset(
                order = "topological",
                transitive = [arg.dynamic_linking.libraries_to_link for arg in args],
            ),
            dynamic_libraries_for_runtime = depset(
                order = "topological",
                transitive = [arg.dynamic_linking.dynamic_libraries_for_runtime for arg in args],
            ),
            user_link_flags = depset(
                order = "topological",
                transitive = [arg.dynamic_linking.user_link_flags for arg in args],
            ),
        ),
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
        "direct_prebuilt_deps": "Set of info of direct prebuilt dependencies.",
        "cc_dependencies": "Direct cc library dependencies. See HaskellCcInfo.",
        "transitive_cc_dependencies": "Transitive cc library dependencies. See HaskellCcInfo.",
        "import_dependencies": "Direct haskell_cc_import library dependencies.",
        "transitive_import_dependencies": "Transitive haskell_cc_import library dependencies.",
    },
)

def get_unmangled_libs(ext_libs):
    """Just a dumb helper because skylark doesn’t do lambdas."""
    return [ext_lib.lib for ext_lib in ext_libs]

def get_mangled_libs(ext_libs):
    """Just a dumb helper because skylark doesn’t do lambdas."""
    return [ext_lib.mangled_lib for ext_lib in ext_libs]

def get_libs_for_ghc_linker(hs, build_info):
    """Return all C library dependencies for GHC's linker.

    GHC has it's own builtin linker. It is used for Template Haskell, for GHCi,
    during doctests, etc. GHC's linker differs from the system's dynamic linker
    in some ways. E.g. it strictly assumes that dynamic libraries end on .dylib
    on MacOS.

    Additionally, on MacOS, GHC produces intermediate dynamic objects that load
    C libraries by their unmangled names.

    This function returns a list of all transitive C library dependencies
    (static or dynamic), taking the requirements of GHC's linker into account.

    Args:
      hs: Haskell context.
      build_info: HaskellBinaryInfo provider.

    Returns:
      (library_deps, ld_library_deps)
      library_deps: List of library files suitable for GHC's builtin linker.
      ld_library_deps: List of library files that should be available for
        dynamic loading.
    """
    trans_link_ctx = build_info.transitive_cc_dependencies.dynamic_linking

    unmangled_libs = get_unmangled_libs(trans_link_ctx.libraries_to_link.to_list())
    mangled_libs = get_mangled_libs(trans_link_ctx.libraries_to_link.to_list())
    import_libs = set.to_list(build_info.transitive_import_dependencies)

    _library_deps = mangled_libs + import_libs
    _ld_library_deps = [
        lib
        for lib in mangled_libs + import_libs
        if is_shared_library(lib)
    ]
    if hs.toolchain.is_darwin:
        # GHC's builtin linker requires .dylib files on MacOS.
        library_deps = darwin_convert_to_dylibs(hs, _library_deps)

        # GHC produces intermediate dylibs that load the unmangled libraries.
        ld_library_deps = _ld_library_deps + [
            lib
            for lib in unmangled_libs
            if is_shared_library(lib)
        ]
    else:
        library_deps = _library_deps
        ld_library_deps = _ld_library_deps

    return (library_deps, ld_library_deps)

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
