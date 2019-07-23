"""runghc support"""

load(":private/context.bzl", "render_env")
load(":private/packages.bzl", "expose_packages", "pkg_info_to_compile_flags")
load(
    ":private/path_utils.bzl",
    "link_libraries",
    "ln",
    "target_unique_name",
)
load(
    ":private/set.bzl",
    "set",
)
load(":providers.bzl", "get_ghci_extra_libs")
load("@bazel_skylib//lib:shell.bzl", "shell")

def build_haskell_runghc(
        hs,
        runghc_wrapper,
        user_compile_flags,
        extra_args,
        hs_info,
        cc_info,
        output,
        package_databases,
        version,
        lib_info = None):
    """Build runghc script.

    Args:
      hs: Haskell context.
      hs_info: HaskellInfo.

      package_databases: package caches excluding the cache file of the package
                      we're creating a runghc for.
      lib_info: If we're building runghc for a library target, pass
                HaskellLibraryInfo here, otherwise it should be None.

    Returns:
      None.
    """

    (pkg_info_inputs, args) = pkg_info_to_compile_flags(
        hs,
        pkg_info = expose_packages(
            package_ids = hs.package_ids,
            package_databases = package_databases,
            version = version,
        ),
        prefix = "runghc-",
    )

    if lib_info != None:
        for idir in set.to_list(hs_info.import_dirs):
            args += ["-i{0}".format(idir)]

    (ghci_extra_libs, ghc_env) = get_ghci_extra_libs(
        hs,
        cc_info,
        path_prefix = "$RULES_HASKELL_EXEC_ROOT",
    )
    link_libraries(ghci_extra_libs, args)

    runghc_file = hs.actions.declare_file(target_unique_name(hs, "runghc"))

    # Extra arguments.
    # `compiler flags` is the default set of arguments for runghc,
    # augmented by `extra_args`.
    # The ordering is important, first compiler flags (from toolchain
    # and local rule), then from `extra_args`. This way the more
    # specific arguments are listed last, and then have more priority in
    # GHC.
    # Note that most flags for GHCI do have their negative value, so a
    # negative flag in `extra_args` can disable a positive flag set
    # in `user_compile_flags`, such as `-XNoOverloadedStrings` will disable
    # `-XOverloadedStrings`.
    args += hs.toolchain.compiler_flags + user_compile_flags + hs.toolchain.repl_ghci_args

    # ghc args need to be wrapped up in "--ghc-arg=" when passing to runghc
    runcompile_flags = ["--ghc-arg=%s" % a for a in args]
    runcompile_flags += extra_args

    hs.actions.expand_template(
        template = runghc_wrapper,
        output = runghc_file,
        substitutions = {
            "{ENV}": render_env(ghc_env),
            "{TOOL}": hs.tools.runghc.path,
            "{CC}": hs.toolchain.cc_wrapper.executable.path,
            "{ARGS}": " ".join([shell.quote(a) for a in runcompile_flags]),
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
        package_databases,
        pkg_info_inputs,
        ghci_extra_libs,
        hs_info.source_files,
        hs.toolchain.cc_wrapper.runfiles.files,
    ])
    ln(hs, runghc_file, output, extra_inputs)
