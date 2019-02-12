"""Action for creating packages and registering them with ghc-pkg"""

load("@bazel_skylib//lib:paths.bzl", "paths")
load(":private/path_utils.bzl", "target_unique_name")
load(":private/pkg_id.bzl", "pkg_id")
load(":private/providers.bzl", "get_mangled_libs")
load(":private/set.bzl", "set")
load(":private/path_utils.bzl", "get_lib_name")

def _get_extra_libraries(dep_info):
    """Get directories and library names for extra library dependencies.

    Args:
      dep_info: HaskellBuildInfo provider of the package.

    Returns:
      (dirs, libs):
      dirs: list: Library search directories for extra library dependencies.
      libs: list: Extra library dependencies.
    """
    cc_libs = get_mangled_libs(
        dep_info.cc_dependencies.dynamic_linking.libraries_to_link.to_list(),
    )
    import_libs = set.to_list(dep_info.import_dependencies)

    # The order in which library dependencies are listed is relevant when
    # linking static archives. To maintain the order defined by the input
    # depset we collect the library dependencies in a list, and use a separate
    # set to deduplicate entries.
    seen_libs = set.empty()
    extra_libs = []
    extra_lib_dirs = set.empty()
    for lib in cc_libs + import_libs:
        lib_name = get_lib_name(lib)
        if not set.is_member(seen_libs, lib_name):
            set.mutable_insert(seen_libs, lib_name)
            extra_libs.append(lib_name)
        set.mutable_insert(extra_lib_dirs, lib.dirname)
    return (set.to_list(extra_lib_dirs), extra_libs)

def package(hs, dep_info, interfaces_dir, interfaces_dir_prof, static_library, dynamic_library, exposed_modules_file, other_modules, my_pkg_id, static_library_prof, short_path):
    """Create GHC package using ghc-pkg.

    Args:
      hs: Haskell context.
      interfaces_dir: Directory containing interface files.
      static_library: Static library of the package.
      dynamic_library: Dynamic library of the package.
      static_library_prof: Static library compiled with profiling or None.

    Returns:
      (File, File): GHC package conf file, GHC package cache file
    """
    pkg_db_dir = pkg_id.to_string(my_pkg_id)
    conf_file = hs.actions.declare_file(
        paths.join(pkg_db_dir, "{0}.conf".format(pkg_db_dir)),
    )
    cache_file = hs.actions.declare_file("package.cache", sibling = conf_file)

    import_dir = paths.join(
        "${pkgroot}",
        paths.join(pkg_db_dir, "_iface"),
    )
    interfaces_dirs = [interfaces_dir]

    if interfaces_dir_prof != None:
        import_dir_prof = paths.join(
            "${pkgroot}",
            paths.join(pkg_db_dir, "_iface_prof"),
        )
        interfaces_dirs.append(interfaces_dir_prof)
    else:
        import_dir_prof = ""

    (extra_lib_dirs, extra_libs) = _get_extra_libraries(dep_info)

    metadata_entries = {
        "name": my_pkg_id.name,
        "version": my_pkg_id.version,
        "id": pkg_id.to_string(my_pkg_id),
        "key": pkg_id.to_string(my_pkg_id),
        "exposed": "True",
        "hidden-modules": " ".join(other_modules),
        "import-dirs": " ".join([import_dir, import_dir_prof]),
        "library-dirs": " ".join(["${pkgroot}"] + extra_lib_dirs),
        "dynamic-library-dirs": " ".join(["${pkgroot}"] + extra_lib_dirs),
        "hs-libraries": pkg_id.library_name(hs, my_pkg_id),
        "extra-libraries": " ".join(extra_libs),
        "depends": ", ".join(
            # Prebuilt dependencies are added further down, since their
            # package-ids are not available as strings but in build outputs.
            set.to_list(dep_info.package_ids),
        ),
    }

    # Create a file from which ghc-pkg will create the actual package
    # from. List of exposed modules generated below.
    metadata_file = hs.actions.declare_file(target_unique_name(hs, "metadata"))
    hs.actions.write(
        output = metadata_file,
        content = "\n".join([
            "{0}: {1}".format(k, v)
            for k, v in metadata_entries.items()
            if v
        ]) + "\n",
    )

    # Collect the package id files of all prebuilt dependencies.
    prebuilt_deps_id_files = [
        dep.id_file
        for dep in set.to_list(dep_info.prebuilt_dependencies)
    ]

    # Combine exposed modules and other metadata to form the package
    # configuration file.

    prebuilt_deps_args = hs.actions.args()
    prebuilt_deps_args.add_all([f.path for f in prebuilt_deps_id_files])
    prebuilt_deps_args.use_param_file("%s", use_always = True)
    prebuilt_deps_args.set_param_file_format("multiline")

    hs.actions.run_shell(
        inputs = [metadata_file, exposed_modules_file] + prebuilt_deps_id_files,
        outputs = [conf_file],
        command = """
            cat $1 > $4
            echo "exposed-modules: `cat $2`" >> $4

            # this is equivalent to 'readarray'. We do use 'readarray' in order to
            # support older bash versions.
            while IFS= read -r line; do deps_id_files+=("$line"); done < $3

            if [ ${#deps_id_files[@]} -eq 0 ]; then
              deps=""
            else
              deps=$(cat "${deps_id_files[@]}" | tr '\n' " ")
            fi
            echo "depends: $deps" >> $4
""",
        arguments = [
            metadata_file.path,
            exposed_modules_file.path,
            prebuilt_deps_args,
            conf_file.path,
        ],
        use_default_shell_env = True,
    )

    # Generate a unique file name based on package ID to store packagedb dir name.
    pkgdb_dirname_fname = "ghc-pkg_{}".format(hash("{}".format(my_pkg_id))).replace("-", "_")

    pkgdb_dirname_file = hs.actions.declare_file(pkgdb_dirname_fname)

    # On Windows, standard file path limit is of 260 characters.
    #
    # This limit can be disabled on Windows 10, but the removal of this
    # limitation is not honored by MingW-derived binaries (such as ghc tools,
    # prior to 8.6.x) by default. Since we can't realistically recompile GHC
    # and friends, we instead use a batch script to compute an MS-DOS style
    # "short path", where each segment contains at most 8 characters.
    #
    # See: https://github.com/haskell/cabal/issues/3972 for a related issue on
    # Cabal side.
    if hs.toolchain.is_windows:
        hs.actions.run(
            executable = short_path,
            outputs = [pkgdb_dirname_file],
            arguments = [
                conf_file.dirname,
                pkgdb_dirname_file.path,
            ],
        )
    else:
        hs.actions.write(
            pkgdb_dirname_file,
            conf_file.dirname,
        )

    # Make the call to ghc-pkg and use the package configuration file
    package_path = ":".join([c.dirname for c in set.to_list(dep_info.package_confs)]) + ":"
    hs.actions.run_shell(
        inputs = depset(transitive = [
            set.to_depset(dep_info.package_confs),
            set.to_depset(dep_info.package_caches),
            depset(interfaces_dirs),
            depset([
                input
                for input in [
                    static_library,
                    conf_file,
                    dynamic_library,
                    static_library_prof,
                ]
                if input
            ]),
            depset([hs.tools.ghc_pkg, pkgdb_dirname_file]),
        ]),
        outputs = [cache_file],
        env = {
            "GHC_PACKAGE_PATH": package_path,
        },
        mnemonic = "HaskellRegisterPackage",
        progress_message = "HaskellRegisterPackage {}".format(hs.label),

        # Registration of a new package consists in,
        #
        # 1. copying the registration file into the package db,
        # 2. performing some validation on the registration file content,
        # 3. recaching, i.e. regenerating the package db cache file.
        #
        # Normally, this is all done by `ghc-pkg register`. But in our
        # case, `ghc-pkg register` is painful, because the validation
        # it performs is slow, somewhat redundant but especially, too
        # strict (see e.g.
        # https://ghc.haskell.org/trac/ghc/ticket/15478). So we do (1)
        # and (3) manually, by copying then calling `ghc-pkg recache`
        # directly.
        #
        # The downside is that we do lose the few validations that
        # `ghc-pkg register` was doing that was useful. e.g. when
        # reexporting modules, validation checks that the source
        # module does exist.
        #
        # TODO Go back to using `ghc-pkg register`. Blocked by
        # https://ghc.haskell.org/trac/ghc/ticket/15478
        command = """
            pkgdb_path=$(< {pkgdb_dirname_file})
            {ghc_pkg} recache\
                "--package-db=$pkgdb_path"\
                -v0\
                --no-expand-pkgroot
        """.format(
            ghc_pkg = hs.tools.ghc_pkg.path,
            pkgdb_dirname_file = pkgdb_dirname_file.path,
        ),
        # XXX: Seems required for this to work on Windows
        use_default_shell_env = True,
    )

    return conf_file, cache_file
