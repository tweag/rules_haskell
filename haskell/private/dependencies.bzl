load("@bazel_skylib//lib:dicts.bzl", "dicts")
load("@bazel_skylib//lib:paths.bzl", "paths")
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

def _cc_add_lib(hs, lib, libs_to_link, libs_for_runtime):
    """Add library to libs_to_link and libs_for_runtime.

    This adds the given library dependency to the list for libraries to link
    and runtime library dependencies.

    On MacOS we apply two patches to the libraries for linking.
    1) GHC's own loader expects dynamic libraries to end on `.dylib`, but Bazel
       produces `.so` files. Here we copy the given library to a `.dylib` file.
    2) MacOS adds load instructions based on the "install name" of the given
       library. The dynamic libraries that Bazel produces maintain their
       package path in their install name. E.g.
       `bazel-out/darwin-fastbuild/bin/some/package/libsomelib.so`.
       Here we modify the install name on a writable copy to be relative to
       @rpath. E.g. `@rpath/libsomelib.so`.

    Args:
      hs: Haskell context.
      lib: The library dependency.
      libs_to_link: (output) list of libraries to link against.
      libs_for_runtime: (output) list of libraries to add to runfiles.
    """
    link_lib = lib
    if is_shared_library(lib):
        libs_for_runtime.append(lib)
        if hs.toolchain.is_darwin:
            link_lib = hs.actions.declare_file(paths.join(
                "_darwin_dylib",
                paths.replace_extension(lib.basename, ".dylib"),
            ))
            hs.actions.run_shell(
                inputs = [lib],
                outputs = [link_lib],
                mnemonic = "HaskellFixupCcInstallName",
                progress_message = "Fixing install name for {0}".format(link_lib.basename),
                command = " &&\n    ".join([
                    "cp {} {}".format(lib.path, link_lib.path),
                    "chmod +w {}".format(link_lib.path),
                    "/usr/bin/install_name_tool -id @rpath/{} {}".format(
                        lib.basename,
                        link_lib.path,
                    ),
                ]),
            )
    libs_to_link.append(link_lib)

def _HaskellCcInfo_from_CcInfo(hs, ctx, cc_info):
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

        _cc_add_lib(hs, static_lib, static_libs_to_link, static_libs_for_runtime)
        _cc_add_lib(hs, dynamic_lib, dynamic_libs_to_link, dynamic_libs_for_runtime)

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
            )
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
            )
        ),
    )

def gather_dep_info(hs, ctx):
    """Collapse dependencies into a single `HaskellBuildInfo`.

    Note that the field `prebuilt_dependencies` also includes
    prebuilt_dependencies of current target.

    Args:
      ctx: Haskell context.
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
            hs_cc_info = _HaskellCcInfo_from_CcInfo(hs, ctx, dep[CcInfo])
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
