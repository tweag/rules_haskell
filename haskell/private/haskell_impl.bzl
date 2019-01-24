"""Implementation of core Haskell rules"""

load(
    "@io_tweag_rules_haskell//haskell:private/providers.bzl",
    "C2hsLibraryInfo",
    "HaskellBinaryInfo",
    "HaskellBuildInfo",
    "HaskellLibraryInfo",
    "HaskellPrebuiltPackageInfo",
)
load(":cc.bzl", "cc_interop_info")
load(
    ":private/actions/link.bzl",
    "link_binary",
    "link_library_dynamic",
    "link_library_static",
)
load(":private/actions/package.bzl", "package")
load(":private/actions/repl.bzl", "build_haskell_repl")
load(":private/actions/runghc.bzl", "build_haskell_runghc")
load(":private/context.bzl", "haskell_context")
load(":private/dependencies.bzl", "gather_dep_info")
load(":private/java.bzl", "java_interop_info")
load(":private/mode.bzl", "is_profiling_enabled")
load(
    ":private/path_utils.bzl",
    "ln",
    "target_unique_name",
)
load(":private/pkg_id.bzl", "pkg_id")
load(":private/set.bzl", "set")
load(":private/providers.bzl", "external_libraries_get_mangled")

def _prepare_srcs(srcs):
    srcs_files = []
    import_dir_map = {}

    for src in srcs:
        # If it has the "files" attribute, it must be a Target
        if hasattr(src, "files"):
            if C2hsLibraryInfo in src:
                srcs_files += src.files.to_list()
                for f in src.files.to_list():
                    import_dir_map[f] = src[C2hsLibraryInfo].import_dir
            else:
                srcs_files += src.files.to_list()

            # otherwise it's just a file

        else:
            srcs_files.append(src)

    return srcs_files, import_dir_map

def haskell_binary_impl(ctx):
    hs = haskell_context(ctx)
    dep_info = gather_dep_info(ctx)

    # Add any interop info for other languages.
    cc = cc_interop_info(ctx)
    java = java_interop_info(ctx)

    with_profiling = is_profiling_enabled(hs)
    srcs_files, import_dir_map = _prepare_srcs(ctx.attr.srcs)
    compiler_flags = ctx.attr.compiler_flags

    c = hs.toolchain.actions.compile_binary(
        hs,
        cc,
        java,
        dep_info,
        srcs = srcs_files,
        ls_modules = ctx.executable._ls_modules,
        import_dir_map = import_dir_map,
        extra_srcs = depset(ctx.files.extra_srcs),
        compiler_flags = ctx.attr.compiler_flags,
        dynamic = not ctx.attr.linkstatic,
        with_profiling = False,
        main_function = ctx.attr.main_function,
        version = ctx.attr.version,
    )

    c_p = None

    if with_profiling:
        c_p = hs.toolchain.actions.compile_binary(
            hs,
            cc,
            java,
            dep_info,
            srcs = srcs_files,
            ls_modules = ctx.executable._ls_modules,
            import_dir_map = import_dir_map,
            # NOTE We must make the object files compiled without profiling
            # available to this step for TH to work, presumably because GHC is
            # linked against RTS without profiling.
            extra_srcs = depset(transitive = [
                depset(ctx.files.extra_srcs),
                depset([c.objects_dir]),
            ]),
            compiler_flags = ctx.attr.compiler_flags,
            # NOTE We can't have profiling and dynamic code at the
            # same time, see:
            # https://ghc.haskell.org/trac/ghc/ticket/15394
            dynamic = False,
            with_profiling = True,
            main_function = ctx.attr.main_function,
            version = ctx.attr.version,
        )

    binary = link_binary(
        hs,
        cc,
        dep_info,
        ctx.files.extra_srcs,
        ctx.attr.compiler_flags,
        c_p.objects_dir if with_profiling else c.objects_dir,
        dynamic = not ctx.attr.linkstatic,
        with_profiling = with_profiling,
        version = ctx.attr.version,
    )

    solibs = set.union(
        set.map(dep_info.external_libraries, external_libraries_get_mangled),
        dep_info.dynamic_libraries,
    )

    build_info = dep_info  # HaskellBuildInfo
    bin_info = HaskellBinaryInfo(
        import_dirs = c.import_dirs,
        source_files = c.source_files,
        binary = binary,
        ghc_args = c.ghc_args,
        header_files = c.header_files,
        exposed_modules_file = c.exposed_modules_file,
    )
    target_files = depset([binary])

    build_haskell_repl(
        hs,
        ghci_script = ctx.file._ghci_script,
        ghci_repl_wrapper = ctx.file._ghci_repl_wrapper,
        compiler_flags = ctx.attr.compiler_flags,
        repl_ghci_args = ctx.attr.repl_ghci_args,
        output = ctx.outputs.repl,
        package_caches = dep_info.package_caches,
        version = ctx.attr.version,
        build_info = build_info,
        bin_info = bin_info,
    )

    # XXX Temporary backwards compatibility hack. Remove eventually.
    # See https://github.com/tweag/rules_haskell/pull/460.
    ln(hs, ctx.outputs.repl, ctx.outputs.repl_deprecated)

    build_haskell_runghc(
        hs,
        ghci_script = ctx.file._ghci_script,
        runghc_wrapper = ctx.file._runghc_wrapper,
        repl_ghci_args = ctx.attr.repl_ghci_args,
        compiler_flags = ctx.attr.compiler_flags,
        output = ctx.outputs.runghc,
        package_caches = dep_info.package_caches,
        version = ctx.attr.version,
        build_info = build_info,
        bin_info = bin_info,
    )

    return [
        build_info,
        bin_info,
        DefaultInfo(
            executable = binary,
            files = target_files,
            runfiles = ctx.runfiles(
                files = set.to_list(solibs),
                collect_data = True,
            ),
        ),
    ]

def haskell_library_impl(ctx):
    hs = haskell_context(ctx)
    dep_info = gather_dep_info(ctx)
    version = ctx.attr.version if ctx.attr.version else None
    my_pkg_id = pkg_id.new(ctx.label, version)
    with_profiling = is_profiling_enabled(hs)
    with_shared = not ctx.attr.linkstatic

    # Add any interop info for other languages.
    cc = cc_interop_info(ctx)
    java = java_interop_info(ctx)

    srcs_files, import_dir_map = _prepare_srcs(ctx.attr.srcs)
    other_modules = ctx.attr.hidden_modules
    exposed_modules_reexports = _exposed_modules_reexports(ctx.attr.exports)

    c = hs.toolchain.actions.compile_library(
        hs,
        cc,
        java,
        dep_info,
        srcs = srcs_files,
        ls_modules = ctx.executable._ls_modules,
        other_modules = other_modules,
        exposed_modules_reexports = exposed_modules_reexports,
        import_dir_map = import_dir_map,
        extra_srcs = depset(ctx.files.extra_srcs),
        compiler_flags = ctx.attr.compiler_flags,
        with_shared = with_shared,
        with_profiling = False,
        my_pkg_id = my_pkg_id,
    )

    c_p = None

    if with_profiling:
        c_p = hs.toolchain.actions.compile_library(
            hs,
            cc,
            java,
            dep_info,
            srcs = srcs_files,
            ls_modules = ctx.executable._ls_modules,
            other_modules = other_modules,
            exposed_modules_reexports = exposed_modules_reexports,
            import_dir_map = import_dir_map,
            # NOTE We must make the object files compiled without profiling
            # available to this step for TH to work, presumably because GHC is
            # linked against RTS without profiling.
            extra_srcs = depset(transitive = [
                depset(ctx.files.extra_srcs),
                depset([c.objects_dir]),
            ]),
            compiler_flags = ctx.attr.compiler_flags,
            # NOTE We can't have profiling and dynamic code at the
            # same time, see:
            # https://ghc.haskell.org/trac/ghc/ticket/15394
            with_shared = False,
            with_profiling = True,
            my_pkg_id = my_pkg_id,
        )

    static_library = link_library_static(
        hs,
        cc,
        dep_info,
        c.objects_dir,
        my_pkg_id,
        with_profiling = False,
    )

    if with_shared:
        dynamic_library = link_library_dynamic(
            hs,
            cc,
            dep_info,
            depset(ctx.files.extra_srcs),
            c.objects_dir,
            my_pkg_id,
        )
        dynamic_libraries = set.insert(
            dep_info.dynamic_libraries,
            dynamic_library,
        )
    else:
        dynamic_library = None
        dynamic_libraries = dep_info.dynamic_libraries

    static_library_prof = None
    if with_profiling:
        static_library_prof = link_library_static(
            hs,
            cc,
            dep_info,
            c_p.objects_dir,
            my_pkg_id,
            with_profiling = True,
        )

    conf_file, cache_file = package(
        hs,
        dep_info,
        c.interfaces_dir,
        c_p.interfaces_dir if c_p != None else None,
        static_library,
        dynamic_library,
        c.exposed_modules_file,
        other_modules,
        my_pkg_id,
        static_library_prof = static_library_prof,
    )

    static_libraries_prof = dep_info.static_libraries_prof

    if static_library_prof != None:
        static_libraries_prof = [static_library_prof] + dep_info.static_libraries_prof

    interface_dirs = set.union(
        dep_info.interface_dirs,
        set.singleton(c.interfaces_dir),
    )

    if c_p != None:
        interface_dirs = set.mutable_union(
            interface_dirs,
            set.singleton(c_p.interfaces_dir),
        )

    build_info = HaskellBuildInfo(
        package_ids = set.insert(dep_info.package_ids, pkg_id.to_string(my_pkg_id)),
        package_confs = set.insert(dep_info.package_confs, conf_file),
        package_caches = set.insert(dep_info.package_caches, cache_file),
        # NOTE We have to use lists for static libraries because the order is
        # important for linker. Linker searches for unresolved symbols to the
        # left, i.e. you first feed a library which has unresolved symbols and
        # then you feed the library which resolves the symbols.
        static_libraries = [static_library] + dep_info.static_libraries,
        static_libraries_prof = static_libraries_prof,
        dynamic_libraries = dynamic_libraries,
        interface_dirs = interface_dirs,
        prebuilt_dependencies = dep_info.prebuilt_dependencies,
        external_libraries = dep_info.external_libraries,
        extra_libraries = dep_info.extra_libraries,
    )
    lib_info = HaskellLibraryInfo(
        package_id = pkg_id.to_string(my_pkg_id),
        version = version,
        import_dirs = c.import_dirs,
        ghc_args = c.ghc_args,
        header_files = c.header_files,
        boot_files = c.boot_files,
        source_files = c.source_files,
        exposed_modules_file = c.exposed_modules_file,
        extra_source_files = c.extra_source_files,
    )
    target_files = depset([conf_file, cache_file])

    if hasattr(ctx, "outputs"):
        build_haskell_repl(
            hs,
            ghci_script = ctx.file._ghci_script,
            ghci_repl_wrapper = ctx.file._ghci_repl_wrapper,
            repl_ghci_args = ctx.attr.repl_ghci_args,
            compiler_flags = ctx.attr.compiler_flags,
            output = ctx.outputs.repl,
            package_caches = dep_info.package_caches,
            version = ctx.attr.version,
            build_info = build_info,
            lib_info = lib_info,
        )

        # XXX Temporary backwards compatibility hack. Remove eventually.
        # See https://github.com/tweag/rules_haskell/pull/460.
        ln(hs, ctx.outputs.repl, ctx.outputs.repl_deprecated)

        build_haskell_runghc(
            hs,
            ghci_script = ctx.file._ghci_script,
            runghc_wrapper = ctx.file._runghc_wrapper,
            repl_ghci_args = ctx.attr.repl_ghci_args,
            compiler_flags = ctx.attr.compiler_flags,
            output = ctx.outputs.runghc,
            package_caches = dep_info.package_caches,
            version = ctx.attr.version,
            build_info = build_info,
            lib_info = lib_info,
        )

    default_info = None

    if hasattr(ctx, "runfiles"):
        default_info = DefaultInfo(
            files = target_files,
            runfiles = ctx.runfiles(collect_data = True),
        )
    else:
        default_info = DefaultInfo(
            files = target_files,
        )

    return [
        build_info,
        lib_info,
        default_info,
    ]

def haskell_import_impl(ctx):
    hs = haskell_context(ctx)

    if ctx.attr.package:
        package = ctx.attr.package
    else:
        package = ctx.label.name

    id_file = hs.actions.declare_file(target_unique_name(hs, "id"))
    hs.actions.run_shell(
        inputs = [hs.tools.ghc_pkg],
        outputs = [id_file],
        command = """
        "$1" --simple-output -v1 field "$2" id > "$3"
        """,
        arguments = [
            hs.tools.ghc_pkg.path,
            package,
            id_file.path,
        ],
    )

    prebuilt_package_info = HaskellPrebuiltPackageInfo(
        package = package,
        id_file = id_file,
    )

    return [prebuilt_package_info]

def _exposed_modules_reexports(exports):
    """Creates a ghc-pkg-compatible list of reexport declarations.

    A ghc-pkg registration file declares reexports as part of the
    exposed-modules field in the following format:

    exposed-modules: A, B, C from pkg-c:C, D from pkg-d:Original.D

    Here, the Original.D module from pkg-d is renamed by virtue of a
    different name being used before the "from" keyword.

    This function creates a ghc-pkg-compatible list of reexport declarations
    (as shown above) from a dictionary mapping package targets to "Cabal-style"
    reexported-modules declarations. That is, something like:

    {
      ":pkg-c": "C",
      ":pkg-d": "Original.D as D",
      ":pkg-e": "E1, Original.E2 as E2",
    }

    Args:
      exports: a dictionary mapping package targets to "Cabal-style"
               reexported-modules declarations.

    Returns:
      a ghc-pkg-compatible list of reexport declarations.
    """
    exposed_reexports = []
    for dep, cabal_decls in exports.items():
        for cabal_decl in cabal_decls.split(","):
            stripped_cabal_decl = cabal_decl.strip()
            cabal_decl_parts = stripped_cabal_decl.split(" as ")
            original = cabal_decl_parts[0]
            if len(cabal_decl_parts) == 2:
                reexported = cabal_decl_parts[1]
            else:
                reexported = cabal_decl_parts[0]

            if HaskellPrebuiltPackageInfo in dep:
                pkg = dep[HaskellPrebuiltPackageInfo].package
            elif HaskellLibraryInfo in dep:
                pkg = dep[HaskellLibraryInfo].package_id

            exposed_reexport = "{reexported} from {pkg}:{original}".format(
                reexported = reexported,
                pkg = pkg,
                original = original,
            )

            exposed_reexports.append(exposed_reexport)

    return exposed_reexports
