"""GHCi REPL support"""

load(":private/context.bzl", "render_env")
load(":private/packages.bzl", "expose_packages", "pkg_info_to_compile_flags")
load(
    ":private/path_utils.bzl",
    "get_lib_name",
    "is_shared_library",
    "link_libraries",
    "ln",
    "target_unique_name",
)
load(":providers.bzl", "get_libs_for_ghc_linker")
load(
    ":private/set.bzl",
    "set",
)
load("@bazel_skylib//lib:paths.bzl", "paths")
load("@bazel_skylib//lib:shell.bzl", "shell")

def build_haskell_repl(
        hs,
        ghci_script,
        ghci_repl_wrapper,
        user_compile_flags,
        repl_ghci_args,
        hs_info,
        output,
        package_databases,
        version,
        lib_info = None):
    """Build REPL script.

    Args:
      hs: Haskell context.
      hs_info: HaskellInfo.

      package_databases: package caches excluding the cache file of the package
                      we're creating a REPL for.
      lib_info: If we're building REPL for a library target, pass
                HaskellLibraryInfo here, otherwise it should be None.

    Returns:
      None.
    """

    # The base and directory packages are necessary for the GHCi script we use
    # (loads source files and brings in scope the corresponding modules).
    args = ["-package", "base", "-package", "directory"]

    pkg_ghc_info = expose_packages(
        hs_info,
        lib_info,
        use_direct = False,
        use_my_pkg_id = None,
        custom_package_databases = package_databases,
        version = version,
    )
    args += pkg_info_to_compile_flags(pkg_ghc_info)

    lib_imports = []
    if lib_info != None:
        for idir in set.to_list(hs_info.import_dirs):
            args += ["-i{0}".format(idir)]
            lib_imports.append(idir)

    link_ctx = hs_info.cc_dependencies.dynamic_linking
    libs_to_link = link_ctx.dynamic_libraries_for_runtime.to_list()

    # External C libraries that we need to make available to the REPL.
    libraries = link_libraries(libs_to_link, args)

    # Transitive library dependencies to have in runfiles.
    (library_deps, ld_library_deps, ghc_env) = get_libs_for_ghc_linker(
        hs,
        hs_info.transitive_cc_dependencies,
        path_prefix = "$RULES_HASKELL_EXEC_ROOT",
    )
    library_path = [paths.dirname(lib.path) for lib in library_deps]
    ld_library_path = [paths.dirname(lib.path) for lib in ld_library_deps]

    repl_file = hs.actions.declare_file(target_unique_name(hs, "repl"))

    add_sources = ["*" + f.path for f in set.to_list(hs_info.source_files)]

    ghci_repl_script = hs.actions.declare_file(
        target_unique_name(hs, "ghci-repl-script"),
    )
    hs.actions.expand_template(
        template = ghci_script,
        output = ghci_repl_script,
        substitutions = {
            "{ADD_SOURCES}": " ".join(add_sources),
            "{COMMANDS}": "",
        },
    )

    # Extra arguments.
    # `compiler flags` is the default set of arguments for the repl,
    # augmented by `repl_ghci_args`.
    # The ordering is important, first compiler flags (from toolchain
    # and local rule), then from `repl_ghci_args`. This way the more
    # specific arguments are listed last, and then have more priority in
    # GHC.
    # Note that most flags for GHCI do have their negative value, so a
    # negative flag in `repl_ghci_args` can disable a positive flag set
    # in `user_compile_flags`, such as `-XNoOverloadedStrings` will disable
    # `-XOverloadedStrings`.
    args += hs.toolchain.compiler_flags + user_compile_flags + hs.toolchain.repl_ghci_args + repl_ghci_args

    hs.actions.expand_template(
        template = ghci_repl_wrapper,
        output = repl_file,
        substitutions = {
            "{ENV}": render_env(ghc_env),
            "{TOOL}": hs.tools.ghci.path,
            "{ARGS}": " ".join(
                [
                    "-ghci-script",
                    paths.join("$RULES_HASKELL_EXEC_ROOT", ghci_repl_script.path),
                ] + [
                    shell.quote(a)
                    for a in args
                ],
            ),
        },
        is_executable = True,
    )

    ghc_info = struct(
        has_version = pkg_ghc_info.has_version,
        library_path = library_path,
        ld_library_path = ld_library_path,
        packages = pkg_ghc_info.packages,
        package_ids = pkg_ghc_info.package_ids,
        package_dbs = pkg_ghc_info.package_dbs,
        lib_imports = lib_imports,
        libraries = libraries,
        execs = struct(
            ghc = hs.tools.ghc.path,
            ghci = hs.tools.ghci.path,
            runghc = hs.tools.runghc.path,
        ),
        flags = struct(
            compiler = user_compile_flags,
            toolchain_compiler = hs.toolchain.compiler_flags,
            repl = repl_ghci_args,
            toolchain_repl = hs.toolchain.repl_ghci_args,
        ),
    )
    ghc_info_file = hs.actions.declare_file(
        target_unique_name(hs, "ghc-info"),
    )
    hs.actions.write(
        output = ghc_info_file,
        content = ghc_info.to_json(),
    )

    # XXX We create a symlink here because we need to force
    # hs.tools.ghci and ghci_script and the best way to do that is
    # to use hs.actions.run. That action, in turn must produce
    # a result, so using ln seems to be the only sane choice.
    extra_inputs = depset(transitive = [
        depset([
            hs.tools.ghci,
            ghci_repl_script,
            repl_file,
            ghc_info_file,
        ]),
        set.to_depset(package_databases),
        depset(library_deps),
        depset(ld_library_deps),
        set.to_depset(hs_info.source_files),
    ])
    ln(hs, repl_file, output, extra_inputs)
