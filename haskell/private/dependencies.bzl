load("@bazel_skylib//lib:dicts.bzl", "dicts")
load("@bazel_skylib//lib:paths.bzl", "paths")
load(
    "@io_tweag_rules_haskell//haskell:providers.bzl",
    "HaskellCcInfo",
    "HaskellInfo",
    "HaskellLibraryInfo",
    "HaskellPrebuiltPackageInfo",
    "empty_HaskellCcInfo",
    "merge_HaskellCcInfo",
)
load(
    ":private/path_utils.bzl",
    "get_lib_name",
    "is_shared_library",
    "is_static_library",
    "ln",
)
load(":private/set.bzl", "set")

def _cc_get_static_lib(lib_info):
    """Return the library to use in static linking mode.

    This returns the first available library artifact in the following order:
    - static_library
    - pic_static_library
    - dynamic_library
    - interface_library

    Args:
      lib_info: LibraryToLink provider.

    Returns:
      File: The library to link against in static mode.
    """
    if lib_info.static_library:
        return lib_info.static_library
    elif lib_info.pic_static_library:
        return lib_info.pic_static_library
    elif lib_info.dynamic_library:
        return lib_info.dynamic_library
    else:
        return lib_info.interface_library

def _cc_get_dynamic_lib(lib_info):
    """Return the library to use in dynamic linking mode.

    This returns the first available library artifact in the following order:
    - dynamic_library
    - interface_library
    - pic_static_library
    - static_library

    Args:
      lib_info: LibraryToLink provider.

    Returns:
      File: The library to link against in dynamic mode.
    """
    if lib_info.dynamic_library:
        return lib_info.dynamic_library
    elif lib_info.interface_library:
        return lib_info.interface_library
    elif lib_info.pic_static_library:
        return lib_info.pic_static_library
    else:
        return lib_info.static_library

def _HaskellCcInfo_from_CcInfo(ctx, cc_info):
    libs_to_link = cc_info.linking_context.libraries_to_link
    static_libs_to_link = []
    dynamic_libs_to_link = []
    static_libs_for_runtime = []
    dynamic_libs_for_runtime = []
    for l in libs_to_link:
        _static_lib = _cc_get_static_lib(l)
        dynamic_lib = _cc_get_dynamic_lib(l)

        # Bazel itself only mangles dynamic libraries, not static libraries.
        # However, we need the library name of the static and dynamic version
        # of a library to match so that we can refer to both with one entry in
        # the package configuration file. Here we rename any static archives
        # with mismatching mangled dynamic library name.
        static_name = get_lib_name(_static_lib)
        dynamic_name = get_lib_name(dynamic_lib)
        if static_name != dynamic_name:
            ext = _static_lib.extension
            static_lib = ctx.actions.declare_file(
                "lib%s.%s" % (dynamic_name, ext),
            )
            ln(ctx, _static_lib, static_lib)
        else:
            static_lib = _static_lib

        static_libs_to_link.append(static_lib)
        if is_shared_library(static_lib):
            static_libs_for_runtime.append(static_lib)
        dynamic_libs_to_link.append(dynamic_lib)
        if is_shared_library(dynamic_lib):
            dynamic_libs_for_runtime.append(dynamic_lib)

    return HaskellCcInfo(
        static_linking = struct(
            libraries_to_link = depset(
                direct = static_libs_to_link,
                order = "topological",
            ),
            dynamic_libraries_for_runtime = depset(
                direct = static_libs_for_runtime,
                order = "topological",
            ),
            user_link_flags = depset(
                direct = cc_info.linking_context.user_link_flags,
                order = "topological",
            ),
        ),
        dynamic_linking = struct(
            libraries_to_link = depset(
                direct = dynamic_libs_to_link,
                order = "topological",
            ),
            dynamic_libraries_for_runtime = depset(
                direct = dynamic_libs_for_runtime,
                order = "topological",
            ),
            user_link_flags = depset(
                direct = cc_info.linking_context.user_link_flags,
                order = "topological",
            ),
        ),
    )

def gather_dep_info(ctx, deps):
    """Collapse dependencies into a single `HaskellInfo`.

    Note that the field `prebuilt_dependencies` also includes
    prebuilt_dependencies of current target.

    Args:
      ctx: Rule context.
      deps: deps attribute.

    Returns:
      HaskellInfo: Unified information about all dependencies.
    """

    acc = HaskellInfo(
        package_ids = set.empty(),
        package_databases = set.empty(),
        version_macros = set.empty(),
        static_libraries = [],
        static_libraries_prof = [],
        dynamic_libraries = set.empty(),
        interface_dirs = set.empty(),
        prebuilt_dependencies = set.empty(),
        direct_prebuilt_deps = set.empty(),
        cc_dependencies = empty_HaskellCcInfo(),
        transitive_cc_dependencies = empty_HaskellCcInfo(),
    )

    for dep in deps:
        if HaskellInfo in dep:
            binfo = dep[HaskellInfo]
            package_ids = acc.package_ids
            if HaskellLibraryInfo not in dep:
                fail("Target {0} cannot depend on binary".format(ctx.attr.name))
            if HaskellLibraryInfo in dep:
                set.mutable_insert(package_ids, dep[HaskellLibraryInfo].package_id)
            acc = HaskellInfo(
                package_ids = package_ids,
                package_databases = set.mutable_union(acc.package_databases, binfo.package_databases),
                version_macros = set.mutable_union(acc.version_macros, binfo.version_macros),
                static_libraries = acc.static_libraries + binfo.static_libraries,
                static_libraries_prof = acc.static_libraries_prof + binfo.static_libraries_prof,
                dynamic_libraries = set.mutable_union(acc.dynamic_libraries, binfo.dynamic_libraries),
                interface_dirs = set.mutable_union(acc.interface_dirs, binfo.interface_dirs),
                prebuilt_dependencies = set.mutable_union(acc.prebuilt_dependencies, binfo.prebuilt_dependencies),
                direct_prebuilt_deps = acc.direct_prebuilt_deps,
                cc_dependencies = acc.cc_dependencies,
                transitive_cc_dependencies = merge_HaskellCcInfo(acc.transitive_cc_dependencies, binfo.transitive_cc_dependencies),
            )
        elif HaskellPrebuiltPackageInfo in dep:
            pkg = dep[HaskellPrebuiltPackageInfo]
            acc = HaskellInfo(
                package_ids = acc.package_ids,
                package_databases = acc.package_databases,
                version_macros = set.mutable_insert(acc.version_macros, pkg.version_macros_file),
                static_libraries = acc.static_libraries,
                static_libraries_prof = acc.static_libraries_prof,
                dynamic_libraries = acc.dynamic_libraries,
                interface_dirs = acc.interface_dirs,
                prebuilt_dependencies = set.mutable_insert(acc.prebuilt_dependencies, pkg),
                direct_prebuilt_deps = set.mutable_insert(acc.direct_prebuilt_deps, pkg),
                cc_dependencies = acc.cc_dependencies,
                transitive_cc_dependencies = acc.transitive_cc_dependencies,
            )
        elif CcInfo in dep and HaskellInfo not in dep:
            # The final link of a binary must include all static libraries we
            # depend on, including transitives ones. Theses libs are provided
            # in the `CcInfo` provider.
            hs_cc_info = _HaskellCcInfo_from_CcInfo(ctx, dep[CcInfo])
            acc = HaskellInfo(
                package_ids = acc.package_ids,
                package_databases = acc.package_databases,
                version_macros = acc.version_macros,
                static_libraries = acc.static_libraries,
                static_libraries_prof = acc.static_libraries_prof,
                dynamic_libraries = acc.dynamic_libraries,
                interface_dirs = acc.interface_dirs,
                prebuilt_dependencies = acc.prebuilt_dependencies,
                direct_prebuilt_deps = acc.direct_prebuilt_deps,
                cc_dependencies = merge_HaskellCcInfo(
                    acc.cc_dependencies,
                    hs_cc_info,
                ),
                transitive_cc_dependencies = merge_HaskellCcInfo(
                    acc.transitive_cc_dependencies,
                    hs_cc_info,
                ),
            )

    return acc
