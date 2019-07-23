"""GHCi REPL support"""

load(":private/context.bzl", "render_env")
load(":private/packages.bzl", "expose_packages", "pkg_info_to_compile_flags")
load(
    ":private/path_utils.bzl",
    "get_lib_name",
    "link_libraries",
    "ln",
    "target_unique_name",
)
load(
    ":private/set.bzl",
    "set",
)
load(":providers.bzl", "get_ghci_extra_libs")
load("@bazel_skylib//lib:paths.bzl", "paths")
load("@bazel_skylib//lib:shell.bzl", "shell")

def build_haskell_repl(
        hs,
        ghci_script,
        ghci_repl_wrapper,
        user_compile_flags,
        repl_ghci_args,
        hs_info,
        cc_info,
        output,
        package_databases,
        version,
        lib_info = None):
    """Build REPL script.

    Args:
      hs: Haskell context.
      hs_info: HaskellInfo.
      cc_info: CcInfo.

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
        package_ids = hs.package_ids,
        package_databases = package_databases,
        version = version,
    )
    (pkg_info_inputs, pkg_info_args) = pkg_info_to_compile_flags(
        hs,
        pkg_info = pkg_ghc_info,
        prefix = "repl-",
    )
    args += pkg_info_args

    lib_imports = []
    if lib_info != None:
        for idir in set.to_list(hs_info.import_dirs):
            if type(idir) != type(""):  # idir can also be a `File`
                idir = idir.path
            args += ["-i{0}".format(idir)]
            lib_imports.append(idir)

    # Link C library dependencies
    (ghci_extra_libs, ghc_env) = get_ghci_extra_libs(
        hs,
        cc_info,
        path_prefix = "$RULES_HASKELL_EXEC_ROOT",
    )
    link_libraries(ghci_extra_libs, args)

    # NOTE: We can avoid constructing this in the future by instead generating
    #   a dedicated package configuration file defining the required libraries.
    ghci_extra_libs_list = ghci_extra_libs.to_list()
    library_path = [lib.dirname for lib in ghci_extra_libs_list]
    libraries = [get_lib_name(lib) for lib in ghci_extra_libs_list]

    repl_file = hs.actions.declare_file(target_unique_name(hs, "repl"))

    add_sources = ["*" + f.path for f in hs_info.source_files.to_list()]

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
            "{CC}": hs.toolchain.cc_wrapper.executable.path,
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
        ld_library_path = library_path,
        package_ids = pkg_ghc_info.package_ids,
        package_dbs = pkg_ghc_info.package_databases,
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
        package_databases,
        pkg_info_inputs,
        ghci_extra_libs,
        hs_info.source_files,
        hs.toolchain.cc_wrapper.runfiles.files,
    ])
    ln(hs, repl_file, output, extra_inputs)
