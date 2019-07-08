"""Implementation of core Haskell rules"""

load(
    "@io_tweag_rules_haskell//haskell:providers.bzl",
    "C2hsLibraryInfo",
    "HaddockInfo",
    "HaskellInfo",
    "HaskellLibraryInfo",
    "HaskellToolchainLibraryInfo",
)
load(":cc.bzl", "cc_interop_info")
load(
    ":private/actions/compile.bzl",
    "list_exposed_modules",
)
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
    "get_dynamic_hs_lib_name",
    "get_static_hs_lib_name",
    "ln",
    "match_label",
    "parse_pattern",
)
load(":private/pkg_id.bzl", "pkg_id")
load(":private/set.bzl", "set")
load(":private/list.bzl", "list")
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

def _resolve_plugin_tools(ctx, plugin_info):
    """Convert a plugin provider to a struct with tools resolved to inputs."""
    (tool_inputs, tool_input_manifests) = ctx.resolve_tools(tools = plugin_info.tools)
    return struct(
        module = plugin_info.module,
        deps = plugin_info.deps,
        args = plugin_info.args,
        tool_inputs = tool_inputs,
        tool_input_manifests = tool_input_manifests,
    )

def _resolve_preprocessors(ctx, preprocessors):
    if not hasattr(ctx, "resolve_tools"):
        # No resolve_tools when ctx is faked (see protobuf.bzl).
        return struct(
            inputs = depset(),
            input_manifests = [],
        )
    (inputs, input_manifests) = ctx.resolve_tools(tools = preprocessors)
    return struct(
        inputs = inputs,
        input_manifests = input_manifests,
    )

def _expand_make_variables(name, ctx, strings):
    # All labels in all attributes should be location-expandable.
    label_attrs = [
        ctx.attr.srcs,
        ctx.attr.extra_srcs,
        ctx.attr.data,
        ctx.attr.deps,
        ctx.attr.plugins,
        ctx.attr.tools,
    ]
    for attr in label_attrs:
        strings = [ctx.expand_location(str, attr) for str in strings]
    strings = [ctx.expand_make_variables(name, str, {}) for str in strings]
    return strings

def _haskell_binary_common_impl(ctx, is_test):
    hs = haskell_context(ctx)
    dep_info = gather_dep_info(ctx, ctx.attr.deps)
    plugin_dep_info = gather_dep_info(
        ctx,
        [dep for plugin in ctx.attr.plugins for dep in plugin[GhcPluginInfo].deps],
    )
    cc_info = cc_common.merge_cc_infos(
        cc_infos = [
            dep[CcInfo]
            for dep in ctx.attr.deps
            if CcInfo in dep
        ] + [
            dep[CcInfo]
            for plugin in ctx.attr.plugins
            for dep in plugin[GhcPluginInfo].deps
            if CcInfo in dep
        ],
    )
    package_ids = [
        dep[HaskellLibraryInfo].package_id
        for dep in ctx.attr.deps
        if HaskellLibraryInfo in dep
    ]

    # Add any interop info for other languages.
    cc = cc_interop_info(ctx)
    java = java_interop_info(ctx)

    with_profiling = is_profiling_enabled(hs)
    srcs_files, import_dir_map = _prepare_srcs(ctx.attr.srcs)
    inspect_coverage = _should_inspect_coverage(ctx, hs, is_test)

    dynamic = not ctx.attr.linkstatic
    if with_profiling or hs.toolchain.is_windows:
        # NOTE We can't have profiling and dynamic code at the
        # same time, see:
        # https://ghc.haskell.org/trac/ghc/ticket/15394
        # Also, GHC on Windows doesn't support dynamic code
        dynamic = False

    plugins = [_resolve_plugin_tools(ctx, plugin[GhcPluginInfo]) for plugin in ctx.attr.plugins]
    preprocessors = _resolve_preprocessors(ctx, ctx.attr.tools)
    user_compile_flags = _expand_make_variables("compiler_flags", ctx, ctx.attr.compiler_flags)
    c = hs.toolchain.actions.compile_binary(
        hs,
        cc,
        java,
        dep_info,
        plugin_dep_info,
        cc_info,
        srcs = srcs_files,
        ls_modules = ctx.executable._ls_modules,
        import_dir_map = import_dir_map,
        extra_srcs = depset(ctx.files.extra_srcs),
        user_compile_flags = user_compile_flags,
        dynamic = dynamic,
        with_profiling = with_profiling,
        main_function = ctx.attr.main_function,
        version = ctx.attr.version,
        inspect_coverage = inspect_coverage,
        plugins = plugins,
        preprocessors = preprocessors,
    )

    # gather intermediary code coverage instrumentation data
    coverage_data = c.coverage_data
    for dep in ctx.attr.deps:
        if HaskellCoverageInfo in dep:
            coverage_data += dep[HaskellCoverageInfo].coverage_data
            coverage_data = list.dedup_on(_get_mix_filepath, coverage_data)

    user_compile_flags = _expand_make_variables("compiler_flags", ctx, ctx.attr.compiler_flags)
    (binary, solibs) = link_binary(
        hs,
        cc,
        dep_info,
        cc_info,
        ctx.files.extra_srcs,
        user_compile_flags,
        c.objects_dir,
        dynamic = dynamic,
        with_profiling = with_profiling,
        version = ctx.attr.version,
    )

    hs_info = HaskellInfo(
        package_databases = dep_info.package_databases,
        version_macros = set.empty(),
        source_files = c.source_files,
        extra_source_files = c.extra_source_files,
        import_dirs = c.import_dirs,
        static_libraries = dep_info.static_libraries,
        dynamic_libraries = dep_info.dynamic_libraries,
        interface_dirs = dep_info.interface_dirs,
        compile_flags = c.compile_flags,
    )
    cc_info = cc_common.merge_cc_infos(
        cc_infos = [dep[CcInfo] for dep in ctx.attr.deps if CcInfo in dep],
    )

    target_files = depset([binary])

    user_compile_flags = _expand_make_variables("compiler_flags", ctx, ctx.attr.compiler_flags)
    repl_ghci_args = _expand_make_variables("repl_ghci_args", ctx, ctx.attr.repl_ghci_args)
    build_haskell_repl(
        hs,
        ghci_script = ctx.file._ghci_script,
        ghci_repl_wrapper = ctx.file._ghci_repl_wrapper,
        user_compile_flags = user_compile_flags,
        repl_ghci_args = repl_ghci_args,
        output = ctx.outputs.repl,
        package_databases = dep_info.package_databases,
        version = ctx.attr.version,
        hs_info = hs_info,
        cc_info = cc_info,
    )

    # XXX Temporary backwards compatibility hack. Remove eventually.
    # See https://github.com/tweag/rules_haskell/pull/460.
    ln(hs, ctx.outputs.repl, ctx.outputs.repl_deprecated)

    user_compile_flags = _expand_make_variables("compiler_flags", ctx, ctx.attr.compiler_flags)
    extra_args = _expand_make_variables("runcompile_flags", ctx, ctx.attr.runcompile_flags)
    build_haskell_runghc(
        hs,
        runghc_wrapper = ctx.file._ghci_repl_wrapper,
        extra_args = extra_args,
        user_compile_flags = user_compile_flags,
        output = ctx.outputs.runghc,
        package_databases = dep_info.package_databases,
        version = ctx.attr.version,
        hs_info = hs_info,
        cc_info = cc_info,
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
                files = extra_runfiles,
                transitive_files = solibs,
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
    cc_info = cc_common.merge_cc_infos(
        cc_infos = [
            dep[CcInfo]
            for dep in ctx.attr.deps
            if CcInfo in dep
        ] + [
            dep[CcInfo]
            for plugin in ctx.attr.plugins
            for dep in plugin[GhcPluginInfo].deps
            if CcInfo in dep
        ],
    )
    package_ids = [
        dep[HaskellLibraryInfo].package_id
        for dep in ctx.attr.deps
        if HaskellLibraryInfo in dep
    ]

    # Add any interop info for other languages.
    cc = cc_interop_info(ctx)
    java = java_interop_info(ctx)

    with_profiling = is_profiling_enabled(hs)
    srcs_files, import_dir_map = _prepare_srcs(ctx.attr.srcs)

    with_shared = not ctx.attr.linkstatic
    if with_profiling or hs.toolchain.is_windows:
        # NOTE We can't have profiling and dynamic code at the
        # same time, see:
        # https://ghc.haskell.org/trac/ghc/ticket/15394
        # Also, GHC on Windows doesn't support dynamic code
        with_shared = False

    package_name = getattr(ctx.attr, "package_name", None)
    version = getattr(ctx.attr, "version", None)
    my_pkg_id = pkg_id.new(ctx.label, package_name, version)

    plugins = [_resolve_plugin_tools(ctx, plugin[GhcPluginInfo]) for plugin in ctx.attr.plugins]
    preprocessors = _resolve_preprocessors(ctx, ctx.attr.tools)
    user_compile_flags = _expand_make_variables("compile_flags", ctx, ctx.attr.compiler_flags)
    c = hs.toolchain.actions.compile_library(
        hs,
        cc,
        java,
        dep_info,
        plugin_dep_info,
        cc_info,
        srcs = srcs_files,
        import_dir_map = import_dir_map,
        extra_srcs = depset(ctx.files.extra_srcs),
        user_compile_flags = user_compile_flags,
        with_shared = with_shared,
        with_profiling = with_profiling,
        my_pkg_id = my_pkg_id,
        plugins = plugins,
        preprocessors = preprocessors,
    )

    other_modules = ctx.attr.hidden_modules
    exposed_modules_reexports = _exposed_modules_reexports(ctx.attr.exports)
    exposed_modules_file = list_exposed_modules(
        hs,
        ls_modules = ctx.executable._ls_modules,
        other_modules = other_modules,
        exposed_modules_reexports = exposed_modules_reexports,
        interfaces_dir = c.interfaces_dir,
        with_profiling = with_profiling,
    )

    static_library = link_library_static(
        hs,
        cc,
        dep_info,
        c.objects_dir,
        my_pkg_id,
        with_profiling = with_profiling,
    )

    if with_shared:
        dynamic_library = link_library_dynamic(
            hs,
            cc,
            dep_info,
            cc_info,
            depset(ctx.files.extra_srcs),
            c.objects_dir,
            my_pkg_id,
        )
        dynamic_libraries = depset([dynamic_library], transitive = [dep_info.dynamic_libraries])
    else:
        dynamic_library = None
        dynamic_libraries = dep_info.dynamic_libraries

    conf_file, cache_file = package(
        hs,
        dep_info,
        cc_info,
        with_shared,
        exposed_modules_file,
        other_modules,
        my_pkg_id,
    )

    interface_dirs = depset(
        direct = [c.interfaces_dir],
        transitive = [dep_info.interface_dirs],
    )

    version_macros = set.empty()
    if version:
        package_name = hs.name
        if hasattr(ctx.attr, "package_name") and ctx.attr.package_name:
            package_name = ctx.attr.package_name
        version_macros = set.singleton(
            generate_version_macros(ctx, package_name, version),
        )

    hs_info = HaskellInfo(
        package_databases = depset([cache_file], transitive = [dep_info.package_databases]),
        version_macros = version_macros,
        source_files = c.source_files,
        extra_source_files = c.extra_source_files,
        import_dirs = c.import_dirs,
        # NOTE We have to use lists for static libraries because the order is
        # important for linker. Linker searches for unresolved symbols to the
        # left, i.e. you first feed a library which has unresolved symbols and
        # then you feed the library which resolves the symbols.
        static_libraries = depset(
            direct = [static_library],
            transitive = [dep_info.static_libraries],
            order = "topological",
        ),
        dynamic_libraries = dynamic_libraries,
        interface_dirs = interface_dirs,
        compile_flags = c.compile_flags,
    )
    lib_info = HaskellLibraryInfo(
        package_id = pkg_id.to_string(my_pkg_id),
        version = version,
    )

    dep_coverage_data = []
    for dep in ctx.attr.deps:
        if HaskellCoverageInfo in dep:
            dep_coverage_data += dep[HaskellCoverageInfo].coverage_data

    coverage_data = dep_coverage_data + c.coverage_data
    coverage_data = list.dedup_on(_get_mix_filepath, coverage_data)

    coverage_info = HaskellCoverageInfo(
        coverage_data = coverage_data,
    )

    target_files = depset([file for file in [static_library, dynamic_library] if file])

    if hasattr(ctx, "outputs"):
        user_compile_flags = _expand_make_variables("compiler_flags", ctx, ctx.attr.compiler_flags)
        repl_ghci_args = _expand_make_variables("repl_ghci_args", ctx, ctx.attr.repl_ghci_args)
        build_haskell_repl(
            hs,
            ghci_script = ctx.file._ghci_script,
            ghci_repl_wrapper = ctx.file._ghci_repl_wrapper,
            repl_ghci_args = repl_ghci_args,
            user_compile_flags = user_compile_flags,
            output = ctx.outputs.repl,
            package_databases = dep_info.package_databases,
            version = ctx.attr.version,
            hs_info = hs_info,
            cc_info = cc_info,
            lib_info = lib_info,
        )

        # XXX Temporary backwards compatibility hack. Remove eventually.
        # See https://github.com/tweag/rules_haskell/pull/460.
        ln(hs, ctx.outputs.repl, ctx.outputs.repl_deprecated)

        extra_args = _expand_make_variables("runcompile_flags", ctx, ctx.attr.runcompile_flags)
        user_compile_flags = _expand_make_variables("compiler_flags", ctx, ctx.attr.compiler_flags)
        build_haskell_runghc(
            hs,
            runghc_wrapper = ctx.file._ghci_repl_wrapper,
            extra_args = extra_args,
            user_compile_flags = user_compile_flags,
            output = ctx.outputs.runghc,
            package_databases = dep_info.package_databases,
            version = ctx.attr.version,
            hs_info = hs_info,
            cc_info = cc_info,
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
        # XXX: protobuf is passing a "patched ctx"
        # which includes the real ctx as "real_ctx"
        ctx = getattr(ctx, "real_ctx", ctx),
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

# We should not need this provider. It exists purely as a workaround
# for https://github.com/bazelbuild/bazel/issues/8129.
#
# TODO Get rid of this by computing a CcInfo in haskell_import
# instead. Currently blocked on upstream.
HaskellImportHack = provider()
HaskellToolchainLibraries = provider()

def haskell_toolchain_library_impl(ctx):
    if ctx.attr.package:
        package = ctx.attr.package
    else:
        package = ctx.label.name

    libraries = ctx.attr._toolchain_libraries[HaskellToolchainLibraries].libraries
    target = libraries.get(package)

    if not target:
        fail(
            """
{} is not a toolchain library.
Check that it ships with your version of GHC.
            """.format(package),
        )

    return [
        target.default_info,
        target.hs_info,
        target.hs_lib_info,
        target.cc_info,
        target.haddock_info,
        HaskellToolchainLibraryInfo(),
    ]

def haskell_toolchain_libraries_impl(ctx):
    hs = haskell_context(ctx)
    with_profiling = is_profiling_enabled(hs)

    # XXX Workaround https://github.com/bazelbuild/bazel/issues/6874.
    # Should be find_cpp_toolchain() instead.
    cc_toolchain = ctx.attr._cc_toolchain[cc_common.CcToolchainInfo]
    feature_configuration = cc_common.configure_features(
        ctx = ctx,
        cc_toolchain = cc_toolchain,
        requested_features = ctx.features,
        unsupported_features = ctx.disabled_features,
    )

    libraries = hs.toolchain.libraries

    # List of library in left-to-right post-ordering
    # Meaning, if package B depends on package A, then A will appear before B.
    ordered = depset(transitive = [
        target[HaskellImportHack].transitive_depends
        for target in hs.toolchain.libraries.values()
    ])

    library_dict = {}
    for package in ordered.to_list():
        target = libraries[package]

        # Construct CcInfo
        if with_profiling:
            # GHC does not provide dynamic profiling mode libraries. The dynamic
            # libraries that are available are missing profiling symbols, that
            # other profiling mode build results will reference. Therefore, we
            # don't import dynamic libraries in profiling mode.
            libs = {
                get_static_hs_lib_name(hs.toolchain.version, lib): {"static": lib}
                for lib in target[HaskellImportHack].static_profiling_libraries.to_list()
            }
        else:
            # Workaround for https://github.com/tweag/rules_haskell/issues/881
            # Static and dynamic libraries don't necessarily pair up 1 to 1.
            # E.g. the rts package in the Unix GHC bindist contains the
            # dynamic libHSrts and the static libCffi and libHSrts.
            libs = {
                get_dynamic_hs_lib_name(hs.toolchain.version, lib): {"dynamic": lib}
                for lib in target[HaskellImportHack].dynamic_libraries.to_list()
            }
            for lib in target[HaskellImportHack].static_libraries.to_list():
                name = get_static_hs_lib_name(with_profiling, lib)
                entry = libs.get(name, {})
                entry["static"] = lib
                libs[name] = entry
        libraries_to_link = [
            cc_common.create_library_to_link(
                actions = ctx.actions,
                feature_configuration = feature_configuration,
                dynamic_library = lib.get("dynamic", None),
                static_library = lib.get("static", None),
                cc_toolchain = cc_toolchain,
            )
            for lib in libs.values()
        ]
        compilation_context = cc_common.create_compilation_context(
            headers = target[HaskellImportHack].headers,
            includes = target[HaskellImportHack].includes,
        )
        linking_context = cc_common.create_linking_context(
            libraries_to_link = libraries_to_link,
            user_link_flags = target[HaskellImportHack].linkopts,
        )
        cc_info = CcInfo(
            compilation_context = compilation_context,
            linking_context = linking_context,
        )
        library_dict[package] = struct(
            default_info = target[DefaultInfo],
            hs_info = target[HaskellInfo],
            hs_lib_info = target[HaskellLibraryInfo],
            cc_info = cc_common.merge_cc_infos(cc_infos = [cc_info] + [
                library_dict[dep].cc_info
                for dep in target[HaskellImportHack].depends
            ]),
            haddock_info = target[HaddockInfo],
        )

    return [HaskellToolchainLibraries(libraries = library_dict)]

def haskell_import_impl(ctx):
    id = ctx.attr.id or ctx.attr.name
    target_files = [
        file
        for file in ctx.files.static_libraries + ctx.files.shared_libraries
    ]
    version_macros = set.empty()
    if ctx.attr.version != None:
        version_macros = set.singleton(
            generate_version_macros(ctx, ctx.label.name, ctx.attr.version),
        )
    hs_info = HaskellInfo(
        # XXX Empty set of conf and cache files only works for global db.
        package_databases = depset(),
        version_macros = version_macros,
        source_files = set.empty(),
        import_dirs = depset(),
        extra_source_files = depset(),
        static_libraries = depset(),
        dynamic_libraries = depset(),
        interface_dirs = depset(),
        compile_flags = [],
    )
    import_info = HaskellImportHack(
        # Make sure we're using the same order for dynamic_libraries,
        # static_libraries.
        dynamic_libraries = depset(ctx.files.shared_libraries),
        static_libraries = depset(ctx.files.static_libraries, order = "topological"),
        # NOTE: haskell_import is evaluated as a toolchain rule. Even if we
        # bazel build with -c dbg, this rule is still executed with
        # ctx.var["COMPILATION_MODE"] == "opt". Therefore, we need to carry
        # both profiling and non-profiling libraries forward so that a later
        # haskell_toolchain_library can select the appropriate artifacts.
        static_profiling_libraries = depset(ctx.files.static_profiling_libraries, order = "topological"),
        headers = depset(ctx.files.hdrs),
        includes = depset(ctx.attr.includes),
        linkopts = ctx.attr.linkopts,
        depends = [dep.label.name for dep in ctx.attr.deps],
        transitive_depends = depset(
            direct = [ctx.attr.name],
            transitive = [dep[HaskellImportHack].transitive_depends for dep in ctx.attr.deps],
            order = "postorder",
        ),
    )

    coverage_info = HaskellCoverageInfo(coverage_data = [])
    lib_info = HaskellLibraryInfo(
        package_id = id,
        version = ctx.attr.version,
    )
    default_info = DefaultInfo(
        files = depset(target_files),
    )

    # This package haddock informations
    transitive_html = {id: ctx.file.haddock_html} if ctx.file.haddock_html else {}
    transitive_haddocks = {id: ctx.files.haddock_interfaces}

    # Add dependencies haddock informations
    for dep in ctx.attr.deps:
        transitive_html.update(dep[HaddockInfo].transitive_html)
        transitive_haddocks.update(dep[HaddockInfo].transitive_haddocks)

    haddock_info = HaddockInfo(
        package_id = id,
        transitive_html = transitive_html,
        transitive_haddocks = transitive_haddocks,
    )

    return [
        hs_info,
        import_info,
        coverage_info,
        default_info,
        lib_info,
        haddock_info,
    ]

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
            if HaskellLibraryInfo in dep:
                pkg = dep[HaskellLibraryInfo].package_id
            exposed_reexport = "{reexported} from {pkg}:{original}".format(
                reexported = reexported,
                pkg = pkg,
                original = original,
            )
            exposed_reexports.append(exposed_reexport)

    return exposed_reexports

def _get_mix_filepath(coverage_datum):
    """ Extracts mix file path from a coverage datum.
    """
    return coverage_datum.mix_file.short_path
