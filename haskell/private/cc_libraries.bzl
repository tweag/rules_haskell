"""Tools for handling and linking static and dynamic libraries.

This includes C and Haskell libraries as both are tracked in CcInfo
providers.
"""

load("@bazel_skylib//lib:dicts.bzl", "dicts")
load("@bazel_skylib//lib:paths.bzl", "paths")
load(
    ":private/packages.bzl",
    "ghc_pkg_recache",
    "write_package_conf",
)
load(
    ":private/path_utils.bzl",
    "create_rpath_entry",
    "get_lib_name",
    "is_hs_library",
    "mangle_static_library",
    "rel_to_pkgroot",
    "target_unique_name",
)
load(
    ":providers.bzl",
    "HaskellCcLibrariesInfo",
    "HaskellCcLibraryInfo",
    "HaskellInfo",
    "HaskellProtobufInfo",
)

def _min_lib_to_link(a, b):
    """Return the smaller of two LibraryToLink objects.

    Determined by component-wise comparison.
    """
    a_tuple = (
        a.dynamic_library,
        a.interface_library,
        a.static_library,
        a.pic_static_library,
    )
    b_tuple = (
        b.dynamic_library,
        b.interface_library,
        b.static_library,
        b.pic_static_library,
    )
    if a_tuple < b_tuple:
        return a
    else:
        return b

def _get_unique_lib_files(cc_info):
    """Deduplicate library dependencies.

    This function removes duplicate filenames in the list of library
    dependencies to avoid clashes when creating symlinks. Such duplicates can
    occur due to dependencies on haskell_toolchain_library targets which each
    duplicate their core library dependencies. See
    https://github.com/tweag/rules_haskell/issues/917.

    This function preserves correct static linking order.

    This function is deterministic in which LibraryToLink is chosen as the
    unique representative independent of their order of appearance.

    Args:
      cc_info: Combined CcInfo provider of dependencies.

    Returns:
      List of LibraryToLink: list of unique libraries to link.
    """
    libs_to_link = cc_info.linking_context.libraries_to_link

    # This is a workaround for duplicated libraries due to
    # haskell_toolchain_library dependencies. See
    # https://github.com/tweag/rules_haskell/issues/917
    libs_by_filename = {}
    filenames = []
    for lib_to_link in libs_to_link.to_list():
        if lib_to_link.dynamic_library:
            lib = lib_to_link.dynamic_library
        elif lib_to_link.interface_library:
            lib = lib_to_link.interface_library
        elif lib_to_link.static_library:
            lib = lib_to_link.static_library
        elif lib_to_link.pic_static_library:
            lib = lib_to_link.pic_static_library
        else:
            fail("Empty CcInfo.linking_context.libraries_to_link entry.")
        prev = libs_by_filename.get(lib.basename)
        if prev:
            # To be deterministic we always use File that compares smaller.
            # This is so that this function can be used multiple times for the
            # same target without generating conflicting actions. E.g. for the
            # compilation step as well as the runghc generation step.
            libs_by_filename[lib.basename] = _min_lib_to_link(prev, lib_to_link)
        else:
            libs_by_filename[lib.basename] = lib_to_link
        filenames.append(lib.basename)

    # Deduplicate the library names. Make sure to preserve static linking order.
    filenames = depset(
        transitive = [depset(direct = [filename]) for filename in filenames],
        order = "topological",
    ).to_list()

    return [
        libs_by_filename[filename]
        for filename in filenames
    ]

def get_ghci_extra_libs(hs, posix, cc_libraries_info, cc_info, path_prefix = None):
    """Get libraries appropriate for GHCi's linker.

    GHC expects dynamic and static versions of the same library to have the
    same library name. Static libraries for which this is not the case will be
    symlinked to a matching name.

    Furthermore, dynamic libraries will be symbolically linked into a common
    directory to allow for less RPATH entries and to fix file extensions that
    GHCi does not support.

    GHCi can load PIC static libraries (-fPIC -fexternal-dynamic-refs) with a
    dynamic RTS and dynamic libraries with a dynamic RTS.

    Args:
      hs: Haskell context.
      cc_libraries_info: Combined HaskellCcLibrariesInfo of dependencies.
      cc_info: Combined CcInfo provider of dependencies.
      path_prefix: (optional) Prefix for the entries in the generated library path.

    Returns:
      libs: depset of File, the libraries that should be passed to GHCi.

    """
    (static_libs, dynamic_libs) = get_extra_libs(
        hs,
        posix,
        cc_libraries_info,
        cc_info,
        dynamic = not hs.toolchain.is_static,
        pic = True,
        fixup_dir = "_ghci_libs",
    )
    libs = depset(transitive = [static_libs, dynamic_libs])

    return libs

def get_extra_libs(hs, posix, cc_libraries_info, cc_info, dynamic = False, pic = None, fixup_dir = "_libs"):
    """Get libraries appropriate for linking with GHC.

    GHC expects dynamic and static versions of the same library to have the
    same library name. Static libraries for which this is not the case will be
    symlinked to a matching name.

    Furthermore, dynamic libraries will be symbolically linked into a common
    directory to allow for less RPATH entries.

    Args:
      hs: Haskell context.
      dynamic: Whether to prefer dynamic libraries.
      cc_libraries_info: Combined HaskellCcLibrariesInfo of dependencies.
      cc_info: Combined CcInfo provider of dependencies.
      dynamic: Whether dynamic libraries are preferred.
      pic: Whether position independent code is required.
      fixup_dir: Generate symbolic links to libraries in this directory.

    Returns:
      depset of File: the libraries that should be passed to GHC for linking.

    """
    fixed_lib_dir = target_unique_name(hs, fixup_dir)
    libs_to_link = _get_unique_lib_files(cc_info)
    static_libs = []
    dynamic_libs = []
    if pic == None:
        pic = dynamic

    # PIC is irrelevant on static GHC.
    pic_required = pic and not hs.toolchain.is_static
    for lib_to_link in libs_to_link:
        cc_library_info = cc_libraries_info.libraries[cc_library_key(lib_to_link)]
        dynamic_lib = None
        if lib_to_link.dynamic_library:
            dynamic_lib = lib_to_link.dynamic_library
        elif lib_to_link.interface_library:
            dynamic_lib = lib_to_link.interface_library
        static_lib = None
        if lib_to_link.pic_static_library:
            static_lib = cc_library_info.pic_static_library_link
            if static_lib == None:
                static_lib = lib_to_link.pic_static_library
        elif lib_to_link.static_library and not pic_required:
            static_lib = cc_library_info.static_library_link
            if static_lib == None:
                static_lib = lib_to_link.static_library

        if static_lib and not (dynamic and dynamic_lib):
            static_libs.append(static_lib)
        elif dynamic_lib:
            dynamic_libs.append(dynamic_lib)
        else:
            # Fall back if no PIC static library is available. This typically
            # happens during profiling builds.
            static_libs.append(lib_to_link.static_library)

    static_libs = depset(direct = static_libs)
    dynamic_libs = depset(direct = dynamic_libs)
    return (static_libs, dynamic_libs)

def create_link_config(hs, posix, cc_libraries_info, cc_info, binary, args, dynamic = None, pic = None):
    """Configure linker flags and inputs.

    Configure linker flags for C library dependencies and runtime dynamic
    library dependencies. And collect the C libraries to pass as inputs to
    the linking action. Creates a package configuration file that captures
    these flags.

    Args:
      hs: Haskell context.
      cc_libraries_info: Combined HaskellCcLibrariesInfo of dependencies.
      cc_info: Combined CcInfo of dependencies.
      binary: Final linked binary.
      args: Arguments to the linking action.
      dynamic: Whether to link dynamically, or statically.
      pic: Whether position independent code is required.

    Returns:
      (cache_file, static_libs, dynamic_libs):
        cache_file: File, the cached package configuration.
        static_libs: depset of File, static library files.
        dynamic_libs: depset of File, dynamic library files.
    """

    (static_libs, dynamic_libs) = get_extra_libs(
        hs,
        posix,
        cc_libraries_info,
        cc_info,
        dynamic = dynamic,
        pic = pic,
    )

    # This test is a hack. When a CC library has a Haskell library
    # as a dependency, we need to be careful to filter it out,
    # otherwise it will end up polluting the linker flags. GHC
    # already uses hs-libraries to link all Haskell libraries.
    #
    # TODO Get rid of this hack. See
    # https://github.com/tweag/rules_haskell/issues/873.
    cc_static_libs = depset(direct = [
        lib
        for lib in static_libs.to_list()
        if not is_hs_library(lib)
    ])
    cc_dynamic_libs = depset(direct = [
        lib
        for lib in dynamic_libs.to_list()
        if not is_hs_library(lib)
    ])

    package_name = target_unique_name(hs, "link-config").replace("_", "-").replace("@", "-")
    conf_path = paths.join(package_name, package_name + ".conf")
    conf_file = hs.actions.declare_file(conf_path)
    libs = cc_static_libs.to_list() + cc_dynamic_libs.to_list()
    write_package_conf(hs, conf_file, {
        "name": package_name,
        "extra-libraries": [
            get_lib_name(lib)
            for lib in libs
        ],
        "library-dirs": depset(direct = [
            rel_to_pkgroot(lib.dirname, conf_file.dirname)
            for lib in libs
        ]),
        "dynamic-library-dirs": depset(direct = [
            rel_to_pkgroot(lib.dirname, conf_file.dirname)
            for lib in libs
        ]),
        # XXX: Set user_link_flags.
        "ld-options": depset(direct = [
            "-Wl,-rpath,%s" % create_rpath_entry(
                binary = binary,
                dependency = lib,
                keep_filename = False,
                prefix = "@loader_path" if hs.toolchain.is_darwin else "$ORIGIN",
            )
            for lib in dynamic_libs.to_list()
        ]),
    })
    cache_file = ghc_pkg_recache(hs, posix, conf_file)

    args.add_all([
        "-package-db",
        cache_file.dirname,
        "-package",
        package_name,
    ])

    return (cache_file, static_libs, dynamic_libs)

def cc_library_key(library_to_link):
    """Convert a LibraryToLink into a hashable dictionary key."""
    return struct(
        dynamic_library = library_to_link.dynamic_library,
        interface_library = library_to_link.interface_library,
        static_library = library_to_link.static_library,
        pic_static_library = library_to_link.pic_static_library,
    )

def deps_HaskellCcLibrariesInfo(deps):
    """Merge the HaskellCcLibrariesInfo over all given dependencies.

    Works on proto_library dependencies as well, where HaskellCcLibrariesInfo
    needs to be constructed by _haskell_proto_aspect.

    Args:
      deps: list of Target, extracts HaskellCcLibrariesInfo from the target
        directly, or from HaskellProtobufInfo if present.

    Returns:
      HaskellCcLibrariesInfo
    """
    infos = []
    for dep in deps:
        if HaskellCcLibrariesInfo in dep:
            infos.append(dep[HaskellCcLibrariesInfo])
        elif HaskellProtobufInfo in dep:
            infos.append(dep[HaskellProtobufInfo].cc_libraries_info)
    return merge_HaskellCcLibrariesInfo(infos = infos)

def merge_HaskellCcLibrariesInfo(infos):
    """Merge multiple HaskellCcLibrariesInfo.

    Prefer deps_HaskellCcLibrariesInfo if possible.
    """
    return HaskellCcLibrariesInfo(
        libraries = dicts.add(*[info.libraries for info in infos]),
    )

def extend_HaskellCcLibrariesInfo(
        ctx,
        cc_libraries_info,
        cc_info,
        is_haskell):
    """Adapt new LibraryToLink and add to HaskellCcLibrariesInfo.

    Generate a new HaskellCcLibraryInfo for each LibraryToLink in cc_info that
    is not already contained in cc_libraries_info and return a new extended
    CcLibrariesInfo.

    Args:
      ctx: Aspect or rule context.
      cc_libraries_info: HaskellCcLibrariesInfo of all dependencies.
      cc_info: CcInfo of the current target.
      is_haskell: Bool, whether the current target is a Haskell library.

    Returns:
      HaskellCcLibrariesInfo
    """
    hs = ctx.toolchains["@rules_haskell//haskell:toolchain"]
    posix = ctx.toolchains["@rules_sh//sh/posix:toolchain_type"]
    libraries = dict(cc_libraries_info.libraries)

    for lib_to_link in cc_info.linking_context.libraries_to_link.to_list():
        key = cc_library_key(lib_to_link)
        if key in libraries:
            continue
        if is_haskell:
            libraries[key] = HaskellCcLibraryInfo(
                static_library_link = None,
                pic_static_library_link = None,
                is_haskell = True,
            )
        else:
            libraries[key] = HaskellCcLibraryInfo(
                static_library_link = mangle_static_library(
                    ctx,
                    posix,
                    lib_to_link.dynamic_library,
                    lib_to_link.static_library,
                    outdir = "_ghc_a",
                ),
                pic_static_library_link = mangle_static_library(
                    ctx,
                    posix,
                    lib_to_link.dynamic_library,
                    lib_to_link.pic_static_library,
                    outdir = "_ghc_pic_a",
                ),
                is_haskell = False,
            )

    return HaskellCcLibrariesInfo(libraries = libraries)

def _haskell_cc_libraries_aspect_impl(target, ctx):
    if HaskellProtobufInfo in target:
        # haskell_cc_libraries_aspect depends on the CcInfo and optionally
        # HaskellInfo providers of a target. In the case of proto_library
        # targets these providers are returned by the _haskell_proto_aspect.
        # That aspect in turn requires HaskellCcLibrariesInfo in all its
        # dependencies. Bazel does not allow this kind of cyclic dependency and
        # one aspect will not be able to observe the other.
        #
        # To work around this we instead generate HaskellCcLibrariesInfo within
        # _haskell_proto_aspect and bundle it in HaskellProtobufInfo.
        return target[HaskellProtobufInfo].cc_libraries_info

    hs = ctx.toolchains["@rules_haskell//haskell:toolchain"]
    posix = ctx.toolchains["@rules_sh//sh/posix:toolchain_type"]

    cc_libraries_info = merge_HaskellCcLibrariesInfo(infos = [
        dep[HaskellCcLibrariesInfo]
        for attr in ["deps", "exports", "plugins"]
        for dep in getattr(ctx.rule.attr, attr, [])
        if HaskellCcLibrariesInfo in dep
    ])

    if CcInfo in target:
        cc_libraries_info = extend_HaskellCcLibrariesInfo(
            ctx = ctx,
            cc_libraries_info = cc_libraries_info,
            cc_info = target[CcInfo],
            is_haskell = HaskellInfo in target,
        )

    return [cc_libraries_info]

haskell_cc_libraries_aspect = aspect(
    implementation = _haskell_cc_libraries_aspect_impl,
    attr_aspects = ["deps", "exports", "plugins"],
    provides = [HaskellCcLibrariesInfo],
    required_aspect_providers = [HaskellProtobufInfo],
    toolchains = [
        "@rules_haskell//haskell:toolchain",
        "@rules_sh//sh/posix:toolchain_type",
    ],
)
"""Extend LibraryToLink of C dependencies for GHC compatibility

Create a symbolic link for each static library whose name doesn't match the
mangled name of the corresponding dynamic library.
"""
