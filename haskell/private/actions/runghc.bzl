"""runghc support"""

load(":private/context.bzl", "render_env")
load(":private/packages.bzl", "expose_packages", "pkg_info_to_ghc_args")
load(
    ":private/path_utils.bzl",
    "get_lib_name",
    "is_shared_library",
    "link_libraries",
    "ln",
    "target_unique_name",
)
load(
    ":private/set.bzl",
    "set",
)
load(":providers.bzl", "get_libs_for_ghc_linker")
load("@bazel_skylib//lib:shell.bzl", "shell")

def build_haskell_runghc(
        hs,
        runghc_wrapper,
        compiler_flags,
        extra_args,
        hs_info,
        output,
        package_databases,
        version,
        lib_info = None,
        bin_info = None):
    """Build runghc script.

    Args:
      hs: Haskell context.
      hs_info: HaskellInfo.

      package_databases: package caches excluding the cache file of the package
                      we're creating a runghc for.
      lib_info: If we're building runghc for a library target, pass
                HaskellLibraryInfo here, otherwise it should be None.
      bin_info: If we're building runghc for a binary target, pass
                HaskellBinaryInfo here, otherwise it should be None.

    Returns:
      None.
    """

    args = pkg_info_to_ghc_args(expose_packages(
        hs_info,
        lib_info,
        use_direct = False,
        use_my_pkg_id = None,
        custom_package_databases = package_databases,
        version = version,
    ))

    if lib_info != None:
        for idir in set.to_list(lib_info.import_dirs):
            args += ["-i{0}".format(idir)]

    link_ctx = hs_info.cc_dependencies.dynamic_linking
    libs_to_link = link_ctx.dynamic_libraries_for_runtime.to_list()

    # External C libraries that we need to make available to runghc.
    link_libraries(libs_to_link, args)

    # Transitive library dependencies to have in runfiles.
    (library_deps, ld_library_deps, ghc_env) = get_libs_for_ghc_linker(
        hs,
        hs_info.transitive_cc_dependencies,
        path_prefix = "$RULES_HASKELL_EXEC_ROOT",
    )

    runghc_file = hs.actions.declare_file(target_unique_name(hs, "runghc"))

    source_files = lib_info.source_files if lib_info != None else bin_info.source_files

    # Extra arguments.
    # `compiler flags` is the default set of arguments for runghc,
    # augmented by `extra_args`.
    # The ordering is important, first compiler flags (from toolchain
    # and local rule), then from `extra_args`. This way the more
    # specific arguments are listed last, and then have more priority in
    # GHC.
    # Note that most flags for GHCI do have their negative value, so a
    # negative flag in `extra_args` can disable a positive flag set
    # in `compiler_flags`, such as `-XNoOverloadedStrings` will disable
    # `-XOverloadedStrings`.
    args += hs.toolchain.compiler_flags + compiler_flags + hs.toolchain.repl_ghci_args

    # ghc args need to be wrapped up in "--ghc-arg=" when passing to runghc
    runghc_args = ["--ghc-arg=%s" % a for a in args]
    runghc_args += extra_args

    hs.actions.expand_template(
        template = runghc_wrapper,
        output = runghc_file,
        substitutions = {
            "{ENV}": render_env(ghc_env),
            "{TOOL}": hs.tools.runghc.path,
            "{ARGS}": " ".join([shell.quote(a) for a in runghc_args]),
        },
        is_executable = True,
    )

    # XXX We create a symlink here because we need to force
    # hs.tools.runghc and the best way to do that is
    # to use hs.actions.run. That action, in turn must produce
    # a result, so using ln seems to be the only sane choice.
    extra_inputs = depset(transitive = [
        depset([
            hs.tools.runghc,
            runghc_file,
        ]),
        set.to_depset(package_databases),
        depset(library_deps),
        depset(ld_library_deps),
        set.to_depset(source_files),
    ])
    ln(hs, runghc_file, output, extra_inputs)
