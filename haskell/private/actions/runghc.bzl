"""runghc support"""

load(":private/packages.bzl", "expose_packages", "pkg_info_to_ghc_args")
load(
    ":private/path_utils.bzl",
    "get_lib_name",
    "is_shared_library",
    "ln",
    "target_unique_name",
)
load(
    ":private/set.bzl",
    "set",
)
load(":private/providers.bzl", "get_libs_for_ghc_linker")
load("@bazel_skylib//lib:shell.bzl", "shell")

def build_haskell_runghc(
        hs,
        runghc_wrapper,
        compiler_flags,
        extra_args,
        build_info,
        output,
        package_caches,
        version,
        lib_info = None,
        bin_info = None):
    """Build runghc script.

    Args:
      hs: Haskell context.
      build_info: HaskellBuildInfo.

      package_caches: package caches excluding the cache file of the package
                      we're creating a runghc for.
      lib_info: If we're building runghc for a library target, pass
                HaskellLibraryInfo here, otherwise it should be None.
      bin_info: If we're building runghc for a binary target, pass
                HaskellBinaryInfo here, otherwise it should be None.

    Returns:
      None.
    """

    args = pkg_info_to_ghc_args(expose_packages(
        build_info,
        lib_info,
        use_direct = False,
        use_my_pkg_id = None,
        custom_package_caches = package_caches,
        version = version,
    ))

    if lib_info != None:
        for idir in set.to_list(lib_info.import_dirs):
            args += ["-i{0}".format(idir)]

    link_ctx = build_info.cc_dependencies.dynamic_linking
    libs_to_link = link_ctx.dynamic_libraries_for_runtime.to_list()

    # External shared libraries that we need to make available to runghc.
    # This only includes dynamic libraries as including static libraries here
    # would cause linking errors as ghci cannot load static libraries.
    # XXX: Verify that static libraries can't be loaded by GHCi.
    seen_libs = set.empty()
    for lib in libs_to_link:
        lib_name = get_lib_name(lib)
        if is_shared_library(lib) and not set.is_member(seen_libs, lib_name):
            set.mutable_insert(seen_libs, lib_name)
            args += ["-l{0}".format(lib_name)]

    # Transitive library dependencies to have in runfiles.
    (library_deps, ld_library_deps, ghc_env) = get_libs_for_ghc_linker(
        hs,
        build_info.transitive_cc_dependencies,
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
            "{LIBPATH}": ghc_env["LIBRARY_PATH"],
            "{LDLIBPATH}": ghc_env["LD_LIBRARY_PATH"],
            "{TOOL}": hs.tools.runghc.path,
            "{SCRIPT_LOCATION}": output.path,
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
        set.to_depset(package_caches),
        depset(library_deps),
        depset(ld_library_deps),
        set.to_depset(source_files),
    ])
    ln(hs, runghc_file, output, extra_inputs)
