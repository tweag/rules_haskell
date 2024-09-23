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
    "get_dirname",
    "get_lib_name",
    "mangle_static_library",
    "rel_to_pkgroot",
    "relative_rpath_prefix",
    "target_unique_name",
)
load(
    ":providers.bzl",
    "HaskellCcLibrariesInfo",
    "HaskellCcLibraryInfo",
    "HaskellInfo",
    "HaskellProtobufInfo",
)

def get_cc_libraries(cc_libraries_info, libraries_to_link):
    """Return only the C libraries out of a list of `LibraryToLink`.

    Filters out libraries that are produced by Haskell target using the
    information provided by `cc_libraries_info`.

    Args:
      cc_libraries_info: Combined HaskellCcLibrariesInfo of dependencies.
      libraries_to_link: list of LibraryToLink.

    Returns:
      list of LibraryToLink, C libraries.

    """
    return [
        lib_to_link
        for lib_to_link in libraries_to_link
        if not cc_libraries_info.libraries[cc_library_key(lib_to_link)].is_haskell
    ]

def get_ghci_library_files(hs, cc_libraries_info, libraries_to_link, *, include_real_paths = False, for_th_only = False):
    """Get libraries appropriate for loading with GHCi.

    See get_library_files for further information.
    """
    static_libs, dynamic_libs = get_library_files(
        hs,
        cc_libraries_info,
        libraries_to_link,
        dynamic = not hs.toolchain.static_runtime,
        pic = True,
        include_real_paths = include_real_paths,
    )
    ghci_can_load_static_libs = hs.toolchain.static_runtime
    if for_th_only and not ghci_can_load_static_libs:
        return dynamic_libs
    return static_libs + dynamic_libs

def get_library_files(hs, cc_libraries_info, libraries_to_link, dynamic = False, pic = None, include_real_paths = False):
    """Get libraries appropriate for linking with GHC.

    Takes a list of LibraryToLink and returns a list of the appropriate
    components for linking with GHC or GHCi.

    GHC expects dynamic and static versions of the same library to have the
    same library name. Static libraries for which this is not the case will be
    symlinked to a matching name.

    Args:
      hs: Haskell context.
      cc_libraries_info: Combined HaskellCcLibrariesInfo of dependencies.
      libraries_to_link: list of LibraryToLink.
      dynamic: Whether dynamic libraries are preferred.
      pic: Whether position independent code is required.
      include_real_paths: Whether to also return the actual path of the
        libraries instead of just their symbolic link in the _solib_k8 directory

    Returns:
      (static_libraries, dynamic_libraries):
        static_libraries: list of File, the static libraries that should be passed to GHC for linking.
        dynamic_libraries: list of File, the dynamic libraries that should be passed to GHC for linking.

    """
    static_libs = []
    dynamic_libs = []
    if pic == None:
        pic = dynamic

    # PIC is irrelevant on static GHC.
    pic_required = pic and not hs.toolchain.static_runtime
    for lib_to_link in libraries_to_link:
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
            extra_lib_files = [
                lib
                for lib in [lib_to_link.resolved_symlink_dynamic_library, lib_to_link.resolved_symlink_interface_library]
                if lib != None
            ]
            if include_real_paths:
                dynamic_libs.extend(extra_lib_files)
        else:
            # Fall back if no PIC static library is available. This typically
            # happens during profiling builds.
            static_libs.append(lib_to_link.static_library)

    return (static_libs, dynamic_libs)

def link_libraries(libs, args, get_dirname = get_dirname, prefix_optl = False):
    """Add linker flags to link against the given libraries.

    This function is intended for linking C library dependencies. Haskell
    libraries are linked by GHC automatically based on `-package(-id)` flags
    and the corresponding package configuration.

    Args:
      libs: Sequence of File, libraries to link.
      args: Args or List, append arguments to this object.
      get_dirname: File -> string, Optionally customize the get_dirname function (for instance to add a prefix to all directories).
      prefix_optl: Bool, whether to prefix linker flags by -optl

    """
    if prefix_optl:
        libfmt = "-optl-l%s"
        dirfmt = "-optl-L%s"
    else:
        libfmt = "-l%s"
        dirfmt = "-L%s"

    if hasattr(args, "add_all"):
        args.add_all(libs, map_each = get_dirname, format_each = dirfmt, uniquify = True)
        args.add_all(libs, map_each = get_lib_name, format_each = libfmt)
    else:
        args.extend([dirfmt % get_dirname(lib) for lib in libs])
        args.extend([libfmt % get_lib_name(lib) for lib in libs])

def create_link_config(hs, posix, cc_libraries_info, libraries_to_link, binary, args, dynamic = None, pic = None, dirprefix = ""):
    """Configure linker flags and inputs.

    Configure linker flags for C library dependencies and runtime dynamic
    library dependencies. And collect the C libraries to pass as inputs to
    the linking action. Creates a package configuration file that captures
    these flags.

    Args:
      hs: Haskell context.
      cc_libraries_info: Combined HaskellCcLibrariesInfo of dependencies.
      libraries_to_link: list of LibraryToLink.
      binary: Final linked binary.
      args: Arguments to the linking action.
      dynamic: Whether to link dynamically, or statically.
      pic: Whether position independent code is required.
      dirprefix: Where to put the config file.

    Returns:
      (cache_file, static_libs, dynamic_libs):
        cache_file: File, the cached package configuration.
        static_libs: list of File, static library files.
        dynamic_libs: list of File, dynamic library files.
    """

    (static_libs, dynamic_libs) = get_library_files(
        hs,
        cc_libraries_info,
        libraries_to_link,
        dynamic = dynamic,
        pic = pic,
    )

    (cc_static_libs, cc_dynamic_libs) = get_library_files(
        hs,
        cc_libraries_info,
        get_cc_libraries(cc_libraries_info, libraries_to_link),
        dynamic = dynamic,
        pic = pic,
    )

    package_name = target_unique_name(hs, "link-config").replace("_", "-").replace("@", "-")
    conf_path = paths.join(dirprefix, package_name, package_name + ".conf")
    conf_file = hs.actions.declare_file(conf_path)
    libs = cc_static_libs + cc_dynamic_libs
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
                prefix = relative_rpath_prefix(hs.toolchain.is_darwin),
            )
            for lib in dynamic_libs
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

def _path_or_none(f):
    if f != None:
        return f.path

def cc_library_key(library_to_link):
    """Convert a LibraryToLink into a hashable dictionary key."""
    return struct(
        dynamic_library = _path_or_none(library_to_link.dynamic_library),
        interface_library = _path_or_none(library_to_link.interface_library),
        static_library = _path_or_none(library_to_link.static_library),
        pic_static_library = _path_or_none(library_to_link.pic_static_library),
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
        cc_shared_info,
        is_haskell):
    """Adapt new LibraryToLink and add to HaskellCcLibrariesInfo.

    Generate a new HaskellCcLibraryInfo for each LibraryToLink in cc_info that
    is not already contained in cc_libraries_info and return a new extended
    CcLibrariesInfo.

    Args:
      ctx: Aspect or rule context.
      cc_libraries_info: HaskellCcLibrariesInfo of all dependencies.
      cc_info: CcInfo of the current target.
      cc_shared_info: CcSharedLibraryInfo of the current target.
      is_haskell: Bool, whether the current target is a Haskell library.

    Returns:
      HaskellCcLibrariesInfo
    """
    posix = ctx.toolchains["@rules_sh//sh/posix:toolchain_type"]
    libraries = dict(cc_libraries_info.libraries)

    linker_inputs = cc_info.linking_context.linker_inputs.to_list() if cc_info else []
    if cc_shared_info:
        linker_inputs.append(cc_shared_info.linker_input)

    for li in linker_inputs:
        for lib_to_link in li.libraries:
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
        # haskell_cc_libraries_aspect depends on the CcInfo, CcSharedLibraryInfo and optionally
        # HaskellInfo providers of a target. In the case of proto_library
        # targets these providers are returned by the _haskell_proto_aspect.
        # That aspect in turn requires HaskellCcLibrariesInfo in all its
        # dependencies. Bazel does not allow this kind of cyclic dependency and
        # one aspect will not be able to observe the other.
        #
        # To work around this we instead generate HaskellCcLibrariesInfo within
        # _haskell_proto_aspect and bundle it in HaskellProtobufInfo.
        return target[HaskellProtobufInfo].cc_libraries_info

    cc_libraries_info = merge_HaskellCcLibrariesInfo(infos = [
        dep[HaskellCcLibrariesInfo]
        for attr in ["deps", "exports", "plugins"]
        for dep in getattr(ctx.rule.attr, attr, [])
        if HaskellCcLibrariesInfo in dep
    ])

    cc_info = target[CcInfo] if CcInfo in target else None
    cc_shared_info = target[CcSharedLibraryInfo] if CcSharedLibraryInfo in target else None
    if cc_info or cc_shared_info:
        cc_libraries_info = extend_HaskellCcLibrariesInfo(
            ctx = ctx,
            cc_libraries_info = cc_libraries_info,
            cc_info = cc_info,
            cc_shared_info = cc_shared_info,
            is_haskell = HaskellInfo in target,
        )

    return [cc_libraries_info]

haskell_cc_libraries_aspect = aspect(
    implementation = _haskell_cc_libraries_aspect_impl,
    attr_aspects = ["deps", "narrowed_deps", "exports", "plugins"],
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

def merge_cc_shared_library_infos(owner, cc_shared_library_infos):
    """Similar to cc_common.merge_cc_infos but for CcSharedLibraryInfo

    Args:
        owner: The label of the target that produced all files used in this input.
        cc_shared_library_infos: CcSharedLibraryInfo providers to be merged.

    Returns:
      CcSharedLibraryInfo
    """
    dynamic_deps = []
    exports = []
    link_once_static_libs = {}
    transitive_dynamic_deps = []
    libraries_to_link = []
    for cc_shared_library_info in cc_shared_library_infos:
        dynamic_dep_entry = struct(
            exports = cc_shared_library_info.exports,
            linker_input = cc_shared_library_info.linker_input,
            link_once_static_libs = cc_shared_library_info.link_once_static_libs,
        )
        dynamic_deps.append(dynamic_dep_entry)
        transitive_dynamic_deps.append(cc_shared_library_info.dynamic_deps)
        libraries_to_link.extend(cc_shared_library_info.linker_input.libraries)
        link_once_static_libs.update(pairs = cc_shared_library_info.link_once_static_libs)
        exports.extend(cc_shared_library_info.exports)

    return CcSharedLibraryInfo(
        dynamic_deps = depset(direct = dynamic_deps, transitive = transitive_dynamic_deps, order = "topological"),
        exports = exports,
        link_once_static_libs = link_once_static_libs,
        linker_input = cc_common.create_linker_input(
            owner = owner,
            libraries = depset(libraries_to_link),
        ),
    )
