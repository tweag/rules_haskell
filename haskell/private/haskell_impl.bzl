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
    "module_name",
    "target_unique_name",
)
load(":private/pkg_id.bzl", "pkg_id")
load(":private/set.bzl", "set")
load(":private/providers.bzl", "HaskellCoverageInfo")
load("@bazel_skylib//lib:paths.bzl", "paths")
load("@bazel_skylib//lib:shell.bzl", "shell")

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

def haskell_test_impl(ctx):
    return _haskell_binary_common_impl(ctx, is_test = True)

def haskell_binary_impl(ctx):
    return _haskell_binary_common_impl(ctx, is_test = False)

def _should_inspect_coverage(ctx, hs, is_test):
    return hs.coverage_enabled and is_test and (ctx.attr.expected_covered_expressions_percentage != None or
                                                ctx.attr.expected_uncovered_expression_count != None)

def _haskell_binary_common_impl(ctx, is_test):
    hs = haskell_context(ctx)
    dep_info = gather_dep_info(ctx)

    # Add any interop info for other languages.
    cc = cc_interop_info(ctx)
    java = java_interop_info(ctx)

    with_profiling = is_profiling_enabled(hs)
    srcs_files, import_dir_map = _prepare_srcs(ctx.attr.srcs)
    compiler_flags = ctx.attr.compiler_flags
    inspect_coverage = _should_inspect_coverage(ctx, hs, is_test)

    mix_files = [module_name(hs, s) + ".mix" for s in srcs_files] if hs.coverage_enabled else []

    c = hs.toolchain.actions.compile_binary(
        hs,
        cc,
        java,
        dep_info,
        srcs = srcs_files,
        ls_modules = ctx.executable._ls_modules,
        import_dir_map = import_dir_map,
        extra_srcs = depset(ctx.files.extra_srcs),
        compiler_flags = compiler_flags,
        dynamic = False if hs.toolchain.is_windows else not ctx.attr.linkstatic,
        with_profiling = False,
        main_function = ctx.attr.main_function,
        version = ctx.attr.version,
        inspect_coverage = inspect_coverage,
        mix_files = mix_files,
    )

    # gather intermediary code coverage instrumentation data
    conditioned_mix_files = c.conditioned_mix_files
    for dep in ctx.attr.deps:
        if HaskellCoverageInfo in dep:
            conditioned_mix_files += dep[HaskellCoverageInfo].mix_files
    conditioned_mix_files = depset(conditioned_mix_files).to_list()

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
            compiler_flags = compiler_flags,
            # NOTE We can't have profiling and dynamic code at the
            # same time, see:
            # https://ghc.haskell.org/trac/ghc/ticket/15394
            dynamic = False,
            with_profiling = True,
            main_function = ctx.attr.main_function,
            version = ctx.attr.version,
            mix_files = mix_files,
        )

    (binary, solibs) = link_binary(
        hs,
        cc,
        dep_info,
        ctx.files.extra_srcs,
        ctx.attr.compiler_flags,
        c_p.objects_dir if with_profiling else c.objects_dir,
        dynamic = False if hs.toolchain.is_windows else not ctx.attr.linkstatic,
        with_profiling = with_profiling,
        version = ctx.attr.version,
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
        compiler_flags = compiler_flags,
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
        runghc_wrapper = ctx.file._ghci_repl_wrapper,
        extra_args = ctx.attr.runghc_args,
        compiler_flags = ctx.attr.compiler_flags,
        output = ctx.outputs.runghc,
        package_caches = dep_info.package_caches,
        version = ctx.attr.version,
        build_info = build_info,
        bin_info = bin_info,
    )

    executable = binary
    extra_runfiles = []

    if inspect_coverage:
        binary_path = paths.join(ctx.workspace_name, binary.short_path)
        hpc_path = paths.join(ctx.workspace_name, hs.toolchain.tools.hpc.short_path)
        tix_file_path = hs.label.name + ".tix"
        mix_file_paths = [
            paths.join(ctx.workspace_name, m.short_path)
            for m in conditioned_mix_files
        ]
        expected_covered_expressions_percentage = ctx.attr.expected_covered_expressions_percentage
        expected_uncovered_expression_count = ctx.attr.expected_uncovered_expression_count
        wrapper = hs.actions.declare_file("coverage_wrapper.sh")
        ctx.actions.expand_template(
            template = ctx.file._coverage_wrapper_template,
            output = wrapper,
            substitutions = {
                "{binary_path}": shell.quote(binary_path),
                "{hpc_path}": shell.quote(hpc_path),
                "{tix_file_path}": shell.quote(tix_file_path),
                "{expected_covered_expressions_percentage}": str(expected_covered_expressions_percentage),
                "{expected_uncovered_expression_count}": str(expected_uncovered_expression_count),
                "{mix_file_paths}": shell.array_literal(mix_file_paths),
            },
            is_executable = True,
        )
        executable = wrapper
        extra_runfiles = [
            ctx.file._bash_runfiles,
            hs.toolchain.tools.hpc,
            binary,
        ]

    return [
        build_info,
        bin_info,
        DefaultInfo(
            executable = executable,
            files = target_files,
            runfiles = ctx.runfiles(
                files =
                    solibs +
                    c.conditioned_mix_files +
                    extra_runfiles,
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
    with_shared = False if hs.toolchain.is_windows else not ctx.attr.linkstatic

    # Add any interop info for other languages.
    cc = cc_interop_info(ctx)
    java = java_interop_info(ctx)

    srcs_files, import_dir_map = _prepare_srcs(ctx.attr.srcs)
    other_modules = ctx.attr.hidden_modules
    exposed_modules_reexports = _exposed_modules_reexports(ctx.attr.exports)

    compiler_flags = ctx.attr.compiler_flags

    mix_files = [module_name(hs, s) + ".mix" for s in srcs_files] if hs.coverage_enabled else []

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
        compiler_flags = compiler_flags,
        with_shared = with_shared,
        with_profiling = False,
        my_pkg_id = my_pkg_id,
        mix_files = mix_files,
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
            compiler_flags = compiler_flags,
            # NOTE We can't have profiling and dynamic code at the
            # same time, see:
            # https://ghc.haskell.org/trac/ghc/ticket/15394
            with_shared = False,
            with_profiling = True,
            my_pkg_id = my_pkg_id,
            mix_files = mix_files,
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
        cc_dependencies = dep_info.cc_dependencies,
        transitive_cc_dependencies = dep_info.transitive_cc_dependencies,
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

    dependency_mix_files = []
    for dep in ctx.attr.deps:
        if HaskellCoverageInfo in dep:
            dependency_mix_files += dep[HaskellCoverageInfo].mix_files

    coverage_info = HaskellCoverageInfo(
        mix_files = depset(dependency_mix_files + c.conditioned_mix_files).to_list(),
    )

    target_files = depset([conf_file, cache_file])

    if hasattr(ctx, "outputs"):
        build_haskell_repl(
            hs,
            ghci_script = ctx.file._ghci_script,
            ghci_repl_wrapper = ctx.file._ghci_repl_wrapper,
            repl_ghci_args = ctx.attr.repl_ghci_args,
            compiler_flags = compiler_flags,
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
            runghc_wrapper = ctx.file._ghci_repl_wrapper,
            extra_args = ctx.attr.runghc_args,
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
        coverage_info,
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
