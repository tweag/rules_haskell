"""GHCi REPL support"""

load(":private/packages.bzl", "expose_packages")
load(
    ":private/path_utils.bzl",
    "get_lib_name",
    "is_shared_library",
    "ln",
    "target_unique_name",
)
load(":private/providers.bzl", "get_libs_for_ghc_linker")
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
        compiler_flags,
        repl_ghci_args,
        build_info,
        output,
        package_caches,
        version,
        lib_info = None,
        bin_info = None):
    """Build REPL script.

    Args:
      hs: Haskell context.
      build_info: HaskellBuildInfo.

      package_caches: package caches excluding the cache file of the package
                      we're creating a REPL for.
      lib_info: If we're building REPL for a library target, pass
                HaskellLibraryInfo here, otherwise it should be None.
      bin_info: If we're building REPL for a binary target, pass
                HaskellBinaryInfo here, otherwise it should be None.

    Returns:
      None.
    """

    # The base and directory packages are necessary for the GHCi script we use
    # (loads source files and brings in scope the corresponding modules).
    args = ["-package", "base", "-package", "directory"]

    args += expose_packages(
        build_info,
        lib_info,
        use_direct = False,
        use_my_pkg_id = None,
        custom_package_caches = package_caches,
        version = version,
    )

    if lib_info != None:
        for idir in set.to_list(lib_info.import_dirs):
            args += ["-i{0}".format(idir)]

    link_ctx = build_info.cc_dependencies.dynamic_linking
    libs_to_link = link_ctx.dynamic_libraries_for_runtime.to_list()
    import_libs_to_link = set.to_list(build_info.import_dependencies)

    # External shared libraries that we need to make available to the REPL.
    # This only includes dynamic libraries as including static libraries here
    # would cause linking errors as ghci cannot load static libraries.
    # XXX: Verify that static libraries can't be loaded by GHCi.
    seen_libs = set.empty()
    for lib in libs_to_link + import_libs_to_link:
        lib_name = get_lib_name(lib)
        if is_shared_library(lib) and not set.is_member(seen_libs, lib_name):
            set.mutable_insert(seen_libs, lib_name)
            args += ["-l{0}".format(lib_name)]

    # Transitive library dependencies to have in runfiles.
    (library_deps, ld_library_deps, ghc_env) = get_libs_for_ghc_linker(
        hs,
        build_info,
        path_prefix = "$RULES_HASKELL_EXEC_ROOT",
    )

    repl_file = hs.actions.declare_file(target_unique_name(hs, "repl"))

    add_sources = []
    if lib_info != None:
        add_sources = ["*" + f.path for f in set.to_list(lib_info.source_files)]
    elif bin_info != None:
        add_sources = ["*" + f.path for f in set.to_list(bin_info.source_files)]

    ghci_repl_script = hs.actions.declare_file(
        target_unique_name(hs, "ghci-repl-script"),
    )
    hs.actions.expand_template(
        template = ghci_script,
        output = ghci_repl_script,
        substitutions = {
            "{ADD_SOURCES}": " ".join(add_sources),
        },
    )

    source_files = lib_info.source_files if lib_info != None else bin_info.source_files

    args += ["-ghci-script", ghci_repl_script.path]

    # Extra arguments.
    # `compiler flags` is the default set of arguments for the repl,
    # augmented by `repl_ghci_args`.
    # The ordering is important, first compiler flags (from toolchain
    # and local rule), then from `repl_ghci_args`. This way the more
    # specific arguments are listed last, and then have more priority in
    # GHC.
    # Note that most flags for GHCI do have their negative value, so a
    # negative flag in `repl_ghci_args` can disable a positive flag set
    # in `compiler_flags`, such as `-XNoOverloadedStrings` will disable
    # `-XOverloadedStrings`.
    args += hs.toolchain.compiler_flags + compiler_flags + hs.toolchain.repl_ghci_args + repl_ghci_args

    hs.actions.expand_template(
        template = ghci_repl_wrapper,
        output = repl_file,
        substitutions = {
            "{LIBPATH}": ghc_env["LIBRARY_PATH"],
            "{LDLIBPATH}": ghc_env["LD_LIBRARY_PATH"],
            "{TOOL}": hs.tools.ghci.path,
            "{SCRIPT_LOCATION}": output.path,
            "{ARGS}": " ".join([shell.quote(a) for a in args]),
        },
        is_executable = True,
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
        ]),
        set.to_depset(package_caches),
        depset(library_deps),
        depset(ld_library_deps),
        set.to_depset(source_files),
    ])
    ln(hs, repl_file, output, extra_inputs)
