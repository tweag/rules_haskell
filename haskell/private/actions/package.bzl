"""Action for creating packages and registering them with ghc-pkg"""

load("@bazel_skylib//lib:paths.bzl", "paths")
load(":private/packages.bzl", "ghc_pkg_recache", "write_package_conf")
load(":private/path_utils.bzl", "get_lib_name", "target_unique_name")
load(":private/pkg_id.bzl", "pkg_id")
load(":private/cc_libraries.bzl", "get_cc_libraries", "get_library_files")

def _get_extra_libraries(hs, with_shared, cc_libraries_info, libraries_to_link, dynamic = False):
    """Get directories and library names for extra library dependencies.

    Args:
      hs: Haskell context.
      posix: POSIX toolchain.
      with_shared: Whether the library is built with both static and shared library outputs.
      cc_libraries_info: HaskellCcLibrariesInfo.
      libraries_to_link: list of LibraryToLink.
      dynamic: Whether to collect dynamic library dependencies.

    Returns:
      (dirs, libs):
      dirs: list: Library search directories for extra library dependencies.
      libs: list: Extra library dependencies.
    """

    # NOTE This is duplicated from path_utils.bzl link_libraries. This whole
    # function can go away once we track libraries outside of package
    # configuration files.
    (cc_static_libs, cc_dynamic_libs) = get_library_files(
        hs,
        cc_libraries_info,
        get_cc_libraries(cc_libraries_info, libraries_to_link),
        pic = with_shared,
        dynamic = dynamic,
    )
    cc_libs = cc_static_libs.to_list() + cc_dynamic_libs.to_list()

    lib_dirs = depset(direct = [
        lib.dirname
        for lib in cc_libs
    ])

    lib_names = [
        get_lib_name(lib)
        for lib in cc_libs
    ]

    return (lib_dirs.to_list(), lib_names)

def package(
        hs,
        posix,
        dep_info,
        cc_libraries_info,
        libraries_to_link,
        with_shared,
        exposed_modules_file,
        other_modules,
        my_pkg_id,
        has_hs_library):
    """Create GHC package using ghc-pkg.

    Args:
      hs: Haskell context.
      posix: POSIX toolchain.
      dep_info: Combined HaskellInfo of dependencies.
      libraries_to_link: list of LibraryToLink.
      with_shared: Whether to link dynamic libraries.
      exposed_modules_file: File holding list of exposed modules.
      other_modules: List of hidden modules.
      my_pkg_id: Package id object for this package.
      has_hs_library: Whether hs-libraries should be non-null.

    Returns:
      (File, File): GHC package conf file, GHC package cache file
    """
    pkg_db_dir = pkg_id.to_string(my_pkg_id)
    conf_file = hs.actions.declare_file(
        paths.join(pkg_db_dir, "{0}.conf".format(pkg_db_dir)),
    )

    import_dir = paths.join(
        "${pkgroot}",
        paths.join(pkg_db_dir, "_iface"),
    )

    (extra_lib_dirs, extra_libs) = _get_extra_libraries(hs, with_shared, cc_libraries_info, libraries_to_link)
    if with_shared:
        (extra_dynamic_lib_dirs, _) = _get_extra_libraries(hs, with_shared, cc_libraries_info, libraries_to_link, dynamic = True)
    else:
        extra_dynamic_lib_dirs = extra_lib_dirs

    # Create a file from which ghc-pkg will create the actual package
    # from. List of exposed modules generated below.
    metadata_file = hs.actions.declare_file(target_unique_name(hs, "metadata"))
    write_package_conf(hs, metadata_file, {
        "name": my_pkg_id.package_name,
        "version": my_pkg_id.version,
        "id": pkg_id.to_string(my_pkg_id),
        "key": pkg_id.to_string(my_pkg_id),
        "exposed": "True",
        "hidden-modules": other_modules,
        "import-dirs": [import_dir],
        "library-dirs": ["${pkgroot}"] + extra_lib_dirs,
        "dynamic-library-dirs": ["${pkgroot}"] + extra_dynamic_lib_dirs,
        "hs-libraries": [pkg_id.library_name(hs, my_pkg_id)] if has_hs_library else [],
        "extra-libraries": extra_libs,
        "depends": hs.package_ids,
    })

    # Combine exposed modules and other metadata to form the package
    # configuration file.

    hs.actions.run_shell(
        inputs = [metadata_file, exposed_modules_file],
        outputs = [conf_file],
        command = """
            "$1" $2 > $4
            echo "exposed-modules: `"$1" $3`" >> $4
""",
        arguments = [
            posix.commands["cat"],
            metadata_file.path,
            exposed_modules_file.path,
            conf_file.path,
        ],
    )

    cache_file = ghc_pkg_recache(hs, posix, conf_file)

    return conf_file, cache_file
