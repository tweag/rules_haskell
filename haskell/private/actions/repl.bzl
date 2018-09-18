"""GHCi REPL support"""

load(
    "@bazel_skylib//:lib.bzl",
    "paths",
    "shell",
)
load(
    ":private/providers.bzl",
    "HaskellBinaryInfo",
    "HaskellBuildInfo",
    "HaskellLibraryInfo",
)
load(
    ":private/path_utils.bzl",
    "get_external_libs_path",
    "get_lib_name",
    "ln",
    "target_unique_name",
)
load(
    ":private/set.bzl",
    "set",
)

def build_haskell_repl(
        hs,
        ghci_script,
        ghci_repl_wrapper,
        compiler_flags,
        repl_ghci_args,
        build_info,
        output,
        package_caches,
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

    # Bring packages in scope.
    args = ["-hide-all-packages"]
    for dep in set.to_list(build_info.prebuilt_dependencies):
        args += ["-package ", dep]
    for package in set.to_list(build_info.package_ids):
        if not (lib_info != None and package == lib_info.package_id):
            args += ["-package-id", package]
    for cache in set.to_list(package_caches):
        args += ["-package-db", cache.dirname]

    if lib_info != None:
        for idir in set.to_list(lib_info.import_dirs):
            args += ["-i{0}".format(idir)]

    # External libraries.
    seen_libs = set.empty()
    for lib in build_info.external_libraries.values():
        lib_name = get_lib_name(lib)
        if not set.is_member(seen_libs, lib_name):
            set.mutable_insert(seen_libs, lib_name)
            args += ["-l{0}".format(lib_name)]

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
            "{LDLIBPATH}": get_external_libs_path(
                set.union(
                    build_info.dynamic_libraries,
                    set.from_list(build_info.external_libraries.values()),
                ),
                prefix = "$RULES_HASKELL_EXEC_ROOT",
            ),
            "{GHCi}": hs.tools.ghci.path,
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
        depset(build_info.external_libraries.values()),
        set.to_depset(source_files),
    ])
    ln(hs, repl_file, output, extra_inputs)
