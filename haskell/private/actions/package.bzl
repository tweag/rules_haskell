"""Action for creating packages and registering them with ghc-pkg"""

load(":private/path_utils.bzl", "target_unique_name")
load(":private/pkg_id.bzl", "pkg_id")
load(":private/set.bzl", "set")
load("@bazel_skylib//:lib.bzl", "paths")

def package(hs, dep_info, interfaces_dir, interfaces_dir_prof, static_library, dynamic_library, exposed_modules_file, other_modules, my_pkg_id, static_library_prof):
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
    pkg_db_dir = my_pkg_id.name
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

    metadata_entries = {
        "name": my_pkg_id.name,
        "version": my_pkg_id.version,
        "id": my_pkg_id.name,
        "key": my_pkg_id.name,
        "exposed": "True",
        "hidden-modules": " ".join(other_modules),
        "import-dirs": " ".join([import_dir, import_dir_prof]),
        "library-dirs": "${pkgroot}",
        "dynamic-library-dirs": "${pkgroot}",
        "hs-libraries": pkg_id.library_name(hs, my_pkg_id),
        "depends": ", ".join(
            # XXX Ideally we would like to specify here prebuilt dependencies
            # too, but we don't know their versions, and package ids without
            # versions will be rejected as unknown.
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

    # Combine exposed modules and other metadata to form the package
    # configuration file.
    hs.actions.run_shell(
        inputs = [metadata_file, exposed_modules_file],
        outputs = [conf_file],
        command = """
        cat $1 > $3
        echo "exposed-modules: $(< $2)" >> $3
        """,
        arguments = [
            metadata_file.path,
            exposed_modules_file.path,
            conf_file.path,
        ],
        use_default_shell_env = True,
    )

    # Make the call to ghc-pkg and use the package configuration file
    package_path = ":".join([c.dirname for c in set.to_list(dep_info.package_confs)]) + ":"
    hs.actions.run(
        inputs = depset(transitive = [
            set.to_depset(dep_info.package_confs),
            set.to_depset(dep_info.package_caches),
            depset(interfaces_dirs),
            depset([static_library, conf_file, dynamic_library] +
                   ([static_library_prof] if static_library_prof != None else [])),
        ]),
        outputs = [cache_file],
        env = {
            "GHC_PACKAGE_PATH": package_path,
        },
        mnemonic = "HaskellRegisterPackage",
        progress_message = "HaskellRegisterPackage {}".format(hs.label),
        executable = hs.tools.ghc_pkg,
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
        arguments = [
            "recache",
            "--package-db={0}".format(conf_file.dirname),
            "-v0",
            "--no-expand-pkgroot",
        ],
    )

    return conf_file, cache_file
