"""Package list handling"""

load(":private/set.bzl", "set")

def expose_packages(build_info, lib_info, use_direct, use_my_pkg_id, custom_package_caches, version):
    """
    Returns the list of command line argument which should be passed
    to GHC in order to enable haskell packages.

    build_info: is common to all builds
    version: if the rule contains a version, we will export the CPP version macro

    All the other arguments are not understood well:

    lib_info: only used for repl and linter
    use_direct: only used for repl and linter
    use_my_pkg_id: only used for one specific task in compile.bzl
    custom_package_caches: override the package_caches of build_info, used only by the repl
    """
    args = [
        # In compile.bzl, we pass this just before all -package-id
        # arguments. Not doing so leads to bizarre compile-time failures.
        # It turns out that equally, not doing so leads to bizarre
        # link-time failures. See
        # https://github.com/tweag/rules_haskell/issues/395.
        "-hide-all-packages",
    ]

    has_version = version != None and version != ""

    if not has_version:
        args.extend([
            # Macro version are disabled for all packages by default
            # and enabled for package with version
            # see https://github.com/tweag/rules_haskell/issues/414
            "-fno-version-macros",
        ])

    # Expose all prebuilt dependencies
    #
    # We have to remember to specify all (transitive) wired-in
    # dependencies or we can't find objects for linking
    #
    # Set use_direct if build_info does not have a direct_prebuilt_deps field.
    for prebuilt_dep in set.to_list(build_info.direct_prebuilt_deps if use_direct else build_info.prebuilt_dependencies):
        args.extend(["-package", prebuilt_dep.package])

    # Expose all bazel dependencies
    for package in set.to_list(build_info.package_ids):
        # XXX: repl and lint uses this lib_info flags
        # It is set to None in all other usage of this function
        # TODO: find the meaning of this flag
        if lib_info == None or package != lib_info.package_id:
            # XXX: use_my_pkg_id is not None only in compile.bzl
            if (use_my_pkg_id == None) or package != use_my_pkg_id:
                args.extend(["-package-id", package])

    # Only include package DBs for deps, prebuilt deps should be found
    # auto-magically by GHC
    for cache in set.to_list(build_info.package_caches if not custom_package_caches else custom_package_caches):
        args.extend(["-package-db", cache.dirname])

    return args
