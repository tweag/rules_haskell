"""Implementation of core Haskell rules"""

load(
    "@io_tweag_rules_haskell//haskell:providers.bzl",
    "C2hsLibraryInfo",
    "HaskellInfo",
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
    "match_label",
    "parse_pattern",
    "target_unique_name",
)
load(":private/pkg_id.bzl", "pkg_id")
load(":private/set.bzl", "set")
load(":private/version_macros.bzl", "generate_version_macros")
load(":providers.bzl", "GhcPluginInfo", "HaskellCoverageInfo")
load("@bazel_skylib//lib:paths.bzl", "paths")
load("@bazel_skylib//lib:collections.bzl", "collections")
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
    return hs.coverage_enabled and is_test

def _coverage_enabled_for_target(coverage_source_patterns, label):
    for pat in coverage_source_patterns:
        if match_label(pat, label):
            return True

    return False

# Mix files refer to genfile srcs including their root. Therefore, we
# must condition the src filepaths passed in for coverage to match.
def _condition_coverage_src(hs, src):
    if not src.path.startswith(hs.genfiles_dir.path):
        return src

    """ Genfiles have the genfile directory as part of their path,
    so declaring a file with the sample path actually makes the new
    file double-qualified by the genfile directory.

    This is necessary because mix files capture the genfile
    path before compilation, and then expect those files to be
    qualified by the genfile directory when `hpc report` or
    `hpc markup` are used. But, genfiles included as runfiles
    are no longer qualified. So, double-qualifying them results in
    only one level of qualification as runfiles.
    """
    conditioned_src = hs.actions.declare_file(src.path)
    hs.actions.run_shell(
        inputs = [src],
        outputs = [conditioned_src],
        arguments = [
            src.path,
            conditioned_src.path,
        ],
        command = """
        mkdir -p $(dirname "$2") && cp "$1" "$2"
        """,
    )

    return conditioned_src

def _haskell_binary_common_impl(ctx, is_test):
    hs = haskell_context(ctx)
    dep_info = gather_dep_info(ctx, ctx.attr.deps)
    plugin_dep_info = gather_dep_info(
        ctx,
        [dep for plugin in ctx.attr.plugins for dep in plugin[GhcPluginInfo].deps],
    )

    # Add any interop info for other languages.
    cc = cc_interop_info(ctx)
    java = java_interop_info(ctx)

    with_profiling = is_profiling_enabled(hs)
    srcs_files, import_dir_map = _prepare_srcs(ctx.attr.srcs)
    inspect_coverage = _should_inspect_coverage(ctx, hs, is_test)

    c = hs.toolchain.actions.compile_binary(
        hs,
        cc,
        java,
        dep_info,
        plugin_dep_info,
        srcs = srcs_files,
        ls_modules = ctx.executable._ls_modules,
        import_dir_map = import_dir_map,
        extra_srcs = depset(ctx.files.extra_srcs),
        user_compile_flags = ctx.attr.compiler_flags,
        dynamic = False if hs.toolchain.is_windows else not ctx.attr.linkstatic,
        with_profiling = False,
        main_function = ctx.attr.main_function,
        version = ctx.attr.version,
        inspect_coverage = inspect_coverage,
        plugins = ctx.attr.plugins,
    )

    # gather intermediary code coverage instrumentation data
    coverage_data = c.coverage_data
    for dep in ctx.attr.deps:
        if HaskellCoverageInfo in dep:
            coverage_data += dep[HaskellCoverageInfo].coverage_data

    c_p = None

    if with_profiling:
        c_p = hs.toolchain.actions.compile_binary(
            hs,
            cc,
            java,
            dep_info,
            plugin_dep_info,
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
            user_compile_flags = ctx.attr.compiler_flags,
            # NOTE We can't have profiling and dynamic code at the
            # same time, see:
            # https://ghc.haskell.org/trac/ghc/ticket/15394
            dynamic = False,
            with_profiling = True,
            main_function = ctx.attr.main_function,
            version = ctx.attr.version,
            plugins = ctx.attr.plugins,
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

    hs_info = HaskellInfo(
        package_ids = dep_info.package_ids,
        package_databases = dep_info.package_databases,
        version_macros = set.empty(),
        source_files = c.source_files,
        extra_source_files = c.extra_source_files,
        import_dirs = c.import_dirs,
        static_libraries = dep_info.static_libraries,
        static_libraries_prof = dep_info.static_libraries_prof,
        dynamic_libraries = dep_info.dynamic_libraries,
        interface_dirs = dep_info.interface_dirs,
        compile_flags = c.compile_flags,
        prebuilt_dependencies = dep_info.prebuilt_dependencies,
        cc_dependencies = dep_info.cc_dependencies,
        transitive_cc_dependencies = dep_info.transitive_cc_dependencies,
    )
    cc_info = cc_common.merge_cc_infos(
        cc_infos = [dep[CcInfo] for dep in ctx.attr.deps if CcInfo in dep],
    )

    target_files = depset([binary])

    build_haskell_repl(
        hs,
        ghci_script = ctx.file._ghci_script,
        ghci_repl_wrapper = ctx.file._ghci_repl_wrapper,
        user_compile_flags = ctx.attr.compiler_flags,
        repl_ghci_args = ctx.attr.repl_ghci_args,
        output = ctx.outputs.repl,
        package_databases = dep_info.package_databases,
        version = ctx.attr.version,
        hs_info = hs_info,
    )

    # XXX Temporary backwards compatibility hack. Remove eventually.
    # See https://github.com/tweag/rules_haskell/pull/460.
    ln(hs, ctx.outputs.repl, ctx.outputs.repl_deprecated)

    build_haskell_runghc(
        hs,
        runghc_wrapper = ctx.file._ghci_repl_wrapper,
        extra_args = ctx.attr.runcompile_flags,
        user_compile_flags = ctx.attr.compiler_flags,
        output = ctx.outputs.runghc,
        package_databases = dep_info.package_databases,
        version = ctx.attr.version,
        hs_info = hs_info,
    )

    executable = binary
    extra_runfiles = []

    if inspect_coverage:
        binary_path = paths.join(ctx.workspace_name, binary.short_path)
        hpc_path = paths.join(ctx.workspace_name, hs.toolchain.tools.hpc.short_path)
        tix_file_path = hs.label.name + ".tix"
        mix_file_paths = [
            paths.join(ctx.workspace_name, datum.mix_file.short_path)
            for datum in coverage_data
        ]
        mix_file_paths = collections.uniq(mix_file_paths)  # remove duplicates

        # find which modules to exclude from coverage analysis, by using the specified source patterns
        raw_coverage_source_patterns = ctx.attr.experimental_coverage_source_patterns
        coverage_source_patterns = [parse_pattern(ctx, pat) for pat in raw_coverage_source_patterns]
        modules_to_exclude = [paths.split_extension(datum.mix_file.basename)[0] for datum in coverage_data if not _coverage_enabled_for_target(coverage_source_patterns, datum.target_label)]
        modules_to_exclude = collections.uniq(modules_to_exclude)  # remove duplicates

        expected_covered_expressions_percentage = ctx.attr.expected_covered_expressions_percentage
        expected_uncovered_expression_count = ctx.attr.expected_uncovered_expression_count
        strict_coverage_analysis = ctx.attr.strict_coverage_analysis
        coverage_report_format = ctx.attr.coverage_report_format

        if coverage_report_format != "text" and coverage_report_format != "html":
            fail("""haskell_test attribute "coverage_report_format" must be one of "text" or "html".""")

        wrapper = hs.actions.declare_file("{}_coverage/coverage_wrapper.sh".format(ctx.label.name))
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
                "{modules_to_exclude}": shell.array_literal(modules_to_exclude),
                "{strict_coverage_analysis}": str(strict_coverage_analysis),
                "{coverage_report_format}": shell.quote(ctx.attr.coverage_report_format),
                "{package_path}": shell.quote(ctx.label.package),
            },
            is_executable = True,
        )
        executable = wrapper
        mix_runfiles = [datum.mix_file for datum in coverage_data]
        srcs_runfiles = [_condition_coverage_src(hs, datum.src_file) for datum in coverage_data]
        extra_runfiles = [
            ctx.file._bash_runfiles,
            hs.toolchain.tools.hpc,
            binary,
        ] + mix_runfiles + srcs_runfiles

    return [
        hs_info,
        cc_info,
        DefaultInfo(
            executable = executable,
            files = target_files,
            runfiles = ctx.runfiles(
                files =
                    solibs +
                    extra_runfiles,
                collect_data = True,
            ),
        ),
    ]

def haskell_library_impl(ctx):
    hs = haskell_context(ctx)
    dep_info = gather_dep_info(ctx, ctx.attr.deps)
    plugin_dep_info = gather_dep_info(
        ctx,
        [dep for plugin in ctx.attr.plugins for dep in plugin[GhcPluginInfo].deps],
    )
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

    c = hs.toolchain.actions.compile_library(
        hs,
        cc,
        java,
        dep_info,
        plugin_dep_info,
        srcs = srcs_files,
        ls_modules = ctx.executable._ls_modules,
        other_modules = other_modules,
        exposed_modules_reexports = exposed_modules_reexports,
        import_dir_map = import_dir_map,
        extra_srcs = depset(ctx.files.extra_srcs),
        user_compile_flags = ctx.attr.compiler_flags,
        with_shared = with_shared,
        with_profiling = False,
        my_pkg_id = my_pkg_id,
        plugins = ctx.attr.plugins,
    )

    c_p = None

    if with_profiling:
        c_p = hs.toolchain.actions.compile_library(
            hs,
            cc,
            java,
            dep_info,
            plugin_dep_info,
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
            user_compile_flags = ctx.attr.compiler_flags,
            # NOTE We can't have profiling and dynamic code at the
            # same time, see:
            # https://ghc.haskell.org/trac/ghc/ticket/15394
            with_shared = False,
            with_profiling = True,
            my_pkg_id = my_pkg_id,
            plugins = ctx.attr.plugins,
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

    version_macros = set.empty()
    if version != None:
        version_macros = set.singleton(
            generate_version_macros(ctx, hs.name, version),
        )

    hs_info = HaskellInfo(
        package_ids = set.insert(dep_info.package_ids, pkg_id.to_string(my_pkg_id)),
        package_databases = set.insert(dep_info.package_databases, cache_file),
        version_macros = version_macros,
        source_files = c.source_files,
        extra_source_files = c.extra_source_files,
        import_dirs = c.import_dirs,
        # NOTE We have to use lists for static libraries because the order is
        # important for linker. Linker searches for unresolved symbols to the
        # left, i.e. you first feed a library which has unresolved symbols and
        # then you feed the library which resolves the symbols.
        static_libraries = [static_library] + dep_info.static_libraries,
        static_libraries_prof = static_libraries_prof,
        dynamic_libraries = dynamic_libraries,
        interface_dirs = interface_dirs,
        compile_flags = c.compile_flags,
        prebuilt_dependencies = dep_info.prebuilt_dependencies,
        cc_dependencies = dep_info.cc_dependencies,
        transitive_cc_dependencies = dep_info.transitive_cc_dependencies,
    )
    lib_info = HaskellLibraryInfo(
        package_id = pkg_id.to_string(my_pkg_id),
        version = version,
    )

    dep_coverage_data = []
    for dep in ctx.attr.deps:
        if HaskellCoverageInfo in dep:
            dep_coverage_data += dep[HaskellCoverageInfo].coverage_data

    coverage_info = HaskellCoverageInfo(
        coverage_data = dep_coverage_data + c.coverage_data,
    )

    target_files = depset([file for file in [static_library, dynamic_library] if file])

    if hasattr(ctx, "outputs"):
        build_haskell_repl(
            hs,
            ghci_script = ctx.file._ghci_script,
            ghci_repl_wrapper = ctx.file._ghci_repl_wrapper,
            repl_ghci_args = ctx.attr.repl_ghci_args,
            user_compile_flags = ctx.attr.compiler_flags,
            output = ctx.outputs.repl,
            package_databases = dep_info.package_databases,
            version = ctx.attr.version,
            hs_info = hs_info,
            lib_info = lib_info,
        )

        # XXX Temporary backwards compatibility hack. Remove eventually.
        # See https://github.com/tweag/rules_haskell/pull/460.
        ln(hs, ctx.outputs.repl, ctx.outputs.repl_deprecated)

        build_haskell_runghc(
            hs,
            runghc_wrapper = ctx.file._ghci_repl_wrapper,
            extra_args = ctx.attr.runcompile_flags,
            user_compile_flags = ctx.attr.compiler_flags,
            output = ctx.outputs.runghc,
            package_databases = dep_info.package_databases,
            version = ctx.attr.version,
            hs_info = hs_info,
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

    # Create a CcInfo provider so that CC rules can work with
    # a haskell library as if it was a regular CC one.

    # XXX Workaround https://github.com/bazelbuild/bazel/issues/6874.
    # Should be find_cpp_toolchain() instead.
    cc_toolchain = ctx.attr._cc_toolchain[cc_common.CcToolchainInfo]
    feature_configuration = cc_common.configure_features(
        cc_toolchain = cc_toolchain,
        requested_features = ctx.features,
        unsupported_features = ctx.disabled_features,
    )
    library_to_link = cc_common.create_library_to_link(
        actions = ctx.actions,
        feature_configuration = feature_configuration,
        dynamic_library = dynamic_library,
        static_library = static_library,
        cc_toolchain = cc_toolchain,
    )
    compilation_context = cc_common.create_compilation_context()
    linking_context = cc_common.create_linking_context(
        libraries_to_link = [library_to_link],
    )
    cc_info = cc_common.merge_cc_infos(
        cc_infos = [
            CcInfo(
                compilation_context = compilation_context,
                linking_context = linking_context,
            ),
        ] + [dep[CcInfo] for dep in ctx.attr.deps if CcInfo in dep],
    )

    return [
        hs_info,
        cc_info,
        coverage_info,
        default_info,
        lib_info,
    ]

def haskell_toolchain_library_impl(ctx):
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

    version_macros_file = hs.actions.declare_file("{}_version_macros.h".format(hs.name))
    hs.actions.run_shell(
        inputs = [hs.tools.ghc_pkg, ctx.executable._version_macros],
        outputs = [version_macros_file],
        command = """
        "$1" \\
            `"$2" --simple-output -v1 field "$3" name` \\
            `"$2" --simple-output -v1 field "$3" version` \\
            > "$4"
        """,
        arguments = [
            ctx.executable._version_macros.path,
            hs.tools.ghc_pkg.path,
            package,
            version_macros_file.path,
        ],
    )

    prebuilt_package_info = HaskellPrebuiltPackageInfo(
        package = package,
        id_file = id_file,
        version_macros_file = version_macros_file,
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
