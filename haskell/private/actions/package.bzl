"""Action for creating packages and registering them with ghc-pkg"""

load(":private/path_utils.bzl", "target_unique_name")
load(":private/pkg_id.bzl", "pkg_id")
load(":private/set.bzl", "set")
load("@bazel_skylib//:lib.bzl", "paths")

def package(hs, dep_info, interfaces_dir, interfaces_dir_prof, static_library, dynamic_library, exposed_modules_file, exposed_modules_reexports, other_modules, my_pkg_id, static_library_prof):
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

    metadata_entries = {
        "name": my_pkg_id.name,
        "version": my_pkg_id.version,
        "id": pkg_id.to_string(my_pkg_id),
        "key": pkg_id.to_string(my_pkg_id),
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
        ]) + "\n",
    )

    # Combine exposed modules and other metadata into a single file.
    registration_file = hs.actions.declare_file(target_unique_name(hs, "registration-file"))
    hs.actions.run_shell(
        inputs = [metadata_file, exposed_modules_file],
        outputs = [registration_file],
        command = """
        cat $1 > $3
        echo "exposed-modules: $(< $2)" >> $3
        echo "exposed-modules: {reexports}" >> $3
        """.format(", ".join(exposed_modules_reexports)),
        arguments = [
            metadata_file.path,
            exposed_modules_file.path,
            registration_file.path,
        ],
        use_default_shell_env = True,
    )

    # Make the call to ghc-pkg and use the registration file
    package_path = ":".join([c.dirname for c in set.to_list(dep_info.package_confs)]) + ":"
    hs.actions.run(
        inputs = depset(transitive = [
            set.to_depset(dep_info.package_confs),
            set.to_depset(dep_info.package_caches),
            depset(interfaces_dirs),
            depset([static_library, registration_file, dynamic_library] +
                   ([static_library_prof] if static_library_prof != None else [])),
        ]),
        outputs = [conf_file, cache_file],
        env = {
            "GHC_PACKAGE_PATH": package_path,
        },
        mnemonic = "HaskellRegisterPackage",
        progress_message = "HaskellRegisterPackage {}".format(hs.label),
        executable = hs.tools.ghc_pkg,
        arguments = [
            "register",
            "--package-db={0}".format(conf_file.dirname),
            "-v0",
            "--no-expand-pkgroot",
            registration_file.path,
        ],
    )

    return conf_file, cache_file
