load("@bazel_skylib//lib:dicts.bzl", "dicts")
load(
    "@io_tweag_rules_haskell//haskell:private/providers.bzl",
    "CcSkylarkApiProviderHacked",
    "HaskellBinaryInfo",
    "HaskellBuildInfo",
    "HaskellCcInfo",
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

def _HaskellCcInfo_from_CcInfo(ctx, cc_info):
    static_linking = cc_info.linking_context.static_mode_params_for_executable
    dynamic_linking = cc_info.linking_context.dynamic_mode_params_for_executable
    manglings = {
        get_lib_name(l.original_artifact()): get_lib_name(l.artifact())
        for l in dynamic_linking.libraries_to_link.to_list()
    }
    static_libs_to_link = []
    for l in static_linking.libraries_to_link.to_list():
        # Bazel itself only mangles dynamic libraries, not static libraries.
        # However, we need the library name of the static and dynamic version
        # of a library to match so that we can refer to both with one entry in
        # the package configuration file. Here we rename any static archives
        # with mismatching mangled dynamic library name.
        orig_lib = l.original_artifact()
        mangled_lib = l.artifact()
        orig_name = get_lib_name(orig_lib)
        mangled_name = get_lib_name(mangled_lib)
        if mangled_name != manglings[orig_name]:
            ext = orig_lib.extension
            link_lib = ctx.actions.declare_file(
                "lib%s.%s" % (manglings[orig_name], ext),
            )
            ln(ctx, orig_lib, link_lib)
            static_libs_to_link.append(struct(
                lib = orig_lib,
                mangled_lib = link_lib,
            ))
        else:
            static_libs_to_link.append(struct(
                lib = orig_lib,
                mangled_lib = mangled_lib,
            ))
    static_libs_to_link = depset(
        direct = static_libs_to_link,
        order = "topological",
    )
    dynamic_libs_to_link = depset(
        direct = [
            struct(
                lib = l.original_artifact(),
                mangled_lib = l.artifact(),
            )
            for l in dynamic_linking.libraries_to_link.to_list()
        ],
        order = "topological",
    )
    return HaskellCcInfo(
        static_linking = struct(
            libraries_to_link = static_libs_to_link,
            dynamic_libraries_for_runtime = static_linking.dynamic_libraries_for_runtime,
            user_link_flags = static_linking.user_link_flags,
        ),
        dynamic_linking = struct(
            libraries_to_link = dynamic_libs_to_link,
            dynamic_libraries_for_runtime = dynamic_linking.dynamic_libraries_for_runtime,
            user_link_flags = dynamic_linking.user_link_flags,
        ),
    )

def gather_dep_info(ctx):
    """Collapse dependencies into a single `HaskellBuildInfo`.

    Note that the field `prebuilt_dependencies` also includes
    prebuilt_dependencies of current target.

    Args:
      ctx: Rule context.

    Returns:
      HaskellBuildInfo: Unified information about all dependencies.
    """

    acc = HaskellBuildInfo(
        package_ids = set.empty(),
        package_confs = set.empty(),
        package_caches = set.empty(),
        static_libraries = [],
        static_libraries_prof = [],
        dynamic_libraries = set.empty(),
        interface_dirs = set.empty(),
        prebuilt_dependencies = set.empty(),
        # a set of struct(lib, mangled_lib)
        direct_prebuilt_deps = set.empty(),
        cc_dependencies = empty_HaskellCcInfo(),
        transitive_cc_dependencies = empty_HaskellCcInfo(),
        import_dependencies = set.empty(),
        transitive_import_dependencies = set.empty(),
    )

    for dep in ctx.attr.deps:
        if HaskellBuildInfo in dep:
            binfo = dep[HaskellBuildInfo]
            package_ids = acc.package_ids
            if HaskellBinaryInfo in dep:
                fail("Target {0} cannot depend on binary".format(ctx.attr.name))
            if HaskellLibraryInfo in dep:
                set.mutable_insert(package_ids, dep[HaskellLibraryInfo].package_id)
            acc = HaskellBuildInfo(
                package_ids = package_ids,
                package_confs = set.mutable_union(acc.package_confs, binfo.package_confs),
                package_caches = set.mutable_union(acc.package_caches, binfo.package_caches),
                static_libraries = acc.static_libraries + binfo.static_libraries,
                static_libraries_prof = acc.static_libraries_prof + binfo.static_libraries_prof,
                dynamic_libraries = set.mutable_union(acc.dynamic_libraries, binfo.dynamic_libraries),
                interface_dirs = set.mutable_union(acc.interface_dirs, binfo.interface_dirs),
                prebuilt_dependencies = set.mutable_union(acc.prebuilt_dependencies, binfo.prebuilt_dependencies),
                direct_prebuilt_deps = acc.direct_prebuilt_deps,
                cc_dependencies = acc.cc_dependencies,
                transitive_cc_dependencies = merge_HaskellCcInfo(acc.transitive_cc_dependencies, binfo.transitive_cc_dependencies),
                import_dependencies = acc.import_dependencies,
                transitive_import_dependencies = set.mutable_union(acc.transitive_import_dependencies, binfo.transitive_import_dependencies),
            )
        elif HaskellPrebuiltPackageInfo in dep:
            pkg = dep[HaskellPrebuiltPackageInfo]
            acc = HaskellBuildInfo(
                package_ids = acc.package_ids,
                package_confs = acc.package_confs,
                package_caches = acc.package_caches,
                static_libraries = acc.static_libraries,
                static_libraries_prof = acc.static_libraries_prof,
                dynamic_libraries = acc.dynamic_libraries,
                interface_dirs = acc.interface_dirs,
                prebuilt_dependencies = set.mutable_insert(acc.prebuilt_dependencies, pkg),
                direct_prebuilt_deps = set.mutable_insert(acc.direct_prebuilt_deps, pkg),
                cc_dependencies = acc.cc_dependencies,
                transitive_cc_dependencies = acc.transitive_cc_dependencies,
                import_dependencies = acc.import_dependencies,
                transitive_import_dependencies = acc.transitive_import_dependencies,
            )
        elif CcInfo in dep:
            # The final link of a binary must include all static libraries we
            # depend on, including transitives ones. Theses libs are provided
            # in the `CcInfo` provider.
            hs_cc_info = _HaskellCcInfo_from_CcInfo(ctx, dep[CcInfo])
            acc = HaskellBuildInfo(
                package_ids = acc.package_ids,
                package_confs = acc.package_confs,
                package_caches = acc.package_caches,
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
                import_dependencies = acc.import_dependencies,
                transitive_import_dependencies = acc.transitive_import_dependencies,
            )
        elif CcSkylarkApiProviderHacked in dep:
            # If the provider is CcSkylarkApiProviderHacked, then the .so
            # files come from haskell_cc_import. In that case there are no
            # indirect shared library dependencies.
            import_deps = set.from_list([
                f
                for f in dep.files.to_list()
                if is_shared_library(f)
            ])

            # If not a Haskell dependency, pass it through as-is to the
            # linking phase.
            acc = HaskellBuildInfo(
                package_ids = acc.package_ids,
                package_confs = acc.package_confs,
                package_caches = acc.package_caches,
                static_libraries = acc.static_libraries,
                static_libraries_prof = acc.static_libraries_prof,
                dynamic_libraries = acc.dynamic_libraries,
                interface_dirs = acc.interface_dirs,
                prebuilt_dependencies = acc.prebuilt_dependencies,
                direct_prebuilt_deps = acc.direct_prebuilt_deps,
                cc_dependencies = acc.cc_dependencies,
                transitive_cc_dependencies = acc.transitive_cc_dependencies,
                import_dependencies = set.mutable_union(
                    acc.import_dependencies,
                    import_deps,
                ),
                transitive_import_dependencies = set.mutable_union(
                    acc.transitive_import_dependencies,
                    import_deps,
                ),
            )

    return acc
