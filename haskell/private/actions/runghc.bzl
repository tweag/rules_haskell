"""GHCi REPL support"""

load(":private/packages.bzl", "expose_packages")
load(
    ":private/path_utils.bzl",
    "get_lib_name",
    "is_shared_library",
    "ln",
    "make_path",
    "target_unique_name",
)
load(
    ":private/set.bzl",
    "set",
)
load(
    ":private/providers.bzl",
    "external_libraries_get_mangled",
)
load("@bazel_skylib//lib:shell.bzl", "shell")

def build_haskell_runghc(
        hs,
        ghci_script,
        runghc_wrapper,
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

    args = expose_packages(
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

    # External shared libraries that we need to make available to the REPL.
    # This is currently the shared libraries made available via haskell_cc_import's.
    # We need to filter out the static libraries as including them here would
    # cause linking errors as ghci cannot load static libraries.
    mangled_external_shared_libraries = \
        [
            e.mangled_lib
            for e in set.to_list(build_info.external_libraries)
            if is_shared_library(e.mangled_lib)
        ]

    seen_libs = set.empty()
    for lib in mangled_external_shared_libraries:
        lib_name = get_lib_name(lib)
        if not set.is_member(seen_libs, lib_name):
            set.mutable_insert(seen_libs, lib_name)
            args += ["-l{0}".format(lib_name)]

    repl_file = hs.actions.declare_file(target_unique_name(hs, "runghc"))

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

    # args += ["-ghci-script", ghci_repl_script.path]

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
    args += hs.toolchain.compiler_flags + compiler_flags + hs.toolchain.repl_ghci_args
    runghc_args = ["--ghc-arg=%s" % a for a in args]
    runghc_args += repl_ghci_args

    hs.actions.expand_template(
        template = runghc_wrapper,
        output = repl_file,
        substitutions = {
            "{LDLIBPATH}": make_path(
                set.union(
                    build_info.dynamic_libraries,
                    set.map(build_info.external_libraries, external_libraries_get_mangled),
                ),
                prefix = "$RULES_HASKELL_EXEC_ROOT",
            ),
            "{RUNGHC}": hs.tools.runghc.path,
            "{SCRIPT_LOCATION}": output.path,
            "{ARGS}": " ".join([shell.quote(a) for a in runghc_args]),
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
        depset(mangled_external_shared_libraries),
        set.to_depset(source_files),
    ])
    ln(hs, repl_file, output, extra_inputs)
