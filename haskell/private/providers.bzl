load(
    ":private/path_utils.bzl",
    "darwin_convert_to_dylibs",
    "make_path",
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
    },
)

def get_libs_for_ghc_linker(hs, transitive_cc_dependencies, path_prefix = None):
    """Return all C library dependencies for GHC's linker.

    GHC has it's own builtin linker. It is used for Template Haskell, for GHCi,
    during doctests, etc. GHC's linker differs from the system's dynamic linker
    in some ways. E.g. it strictly assumes that dynamic libraries end on .dylib
    on MacOS.

    This function returns a list of all transitive C library dependencies
    (static or dynamic), taking the requirements of GHC's linker into account.

    Args:
      hs: Haskell context.
      transitive_cc_dependencies: HaskellCcInfo provider.
      path_prefix: Prefix for paths in GHC environment variables.

    Returns:
      (library_deps, ld_library_deps, env)
      library_deps: List of library files suitable for GHC's builtin linker.
      ld_library_deps: List of library files that should be available for
        dynamic loading.
      env: A mapping environment variables LIBRARY_PATH and LD_LIBRARY_PATH,
        to the corresponding values as expected by GHC.
    """
    trans_link_ctx = transitive_cc_dependencies.dynamic_linking

    libs_to_link = trans_link_ctx.libraries_to_link.to_list()
    libs_for_runtime = trans_link_ctx.dynamic_libraries_for_runtime.to_list()

    _library_deps = libs_to_link
    _ld_library_deps = libs_for_runtime
    if hs.toolchain.is_darwin:
        # GHC's builtin linker requires .dylib files on MacOS.
        library_deps = darwin_convert_to_dylibs(hs, _library_deps)

        # Additionally ghc 8.4 requires library_deps here although 8.6 does not
        ld_library_deps = library_deps + _ld_library_deps
    else:
        library_deps = _library_deps
        ld_library_deps = _ld_library_deps

    library_path = make_path(
        library_deps,
        prefix = path_prefix,
    )
    ld_library_path = make_path(
        ld_library_deps,
        prefix = path_prefix,
    )

    # GHC's builtin linker/loader looks for libraries in the paths defined by
    # LIBRARY_PATH and LD_LIBRARY_PATH.
    # See https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html?highlight=library_path#extra-libraries
    # In certain cases it is not enough to specify LD_LIBRARY_PATH alone, and
    # libraries are only found if their path is included in LIBRARY_PATH.
    # See https://github.com/tweag/rules_haskell/pull/685
    env = {
        "LIBRARY_PATH": library_path,
        "LD_LIBRARY_PATH": ld_library_path,
    }

    return (library_deps, ld_library_deps, env)

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

HaskellCoverageInfo = provider(
    doc = "Information about coverage instrumentation for Haskell files.",
    fields = {
        "coverage_data": "A list of coverage data containing which parts of Haskell source code are being tracked for code coverage.",
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
        "tool_inputs": "Inputs required for plugin tools.",
        "tool_input_manifests": "Plugin tools input manifests.",
    },
)
