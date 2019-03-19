"""filegroup-like target for haskell_library/haskell_binary"""

load(":private/set.bzl", "set")
load(
    ":private/providers.bzl",
    "HaskellBuildInfo",
    "HaskellLibraryInfo",
    "empty_HaskellCcInfo",
    "merge_HaskellCcInfo",
)
load(":private/pkg_id.bzl", "pkg_id")
load(
    ":private/path_utils.bzl",
    "target_unique_name",
)
load(
    ":private/actions/repl.bzl",
    "build_haskell_repl",
)
load(":private/context.bzl", "haskell_context")

def appendBuildInfos(bi1, bi2):
    return HaskellBuildInfo(
        package_ids = set.union(bi1.package_ids, bi2.package_ids),
        package_confs = set.union(bi1.package_confs, bi2.package_confs),
        package_caches = set.union(bi1.package_caches, bi2.package_caches),
        static_libraries = bi1.static_libraries + bi2.static_libraries,
        static_libraries_prof = bi1.static_libraries_prof + bi2.static_libraries_prof,
        dynamic_libraries = set.union(bi1.dynamic_libraries, bi2.dynamic_libraries),
        interface_dirs = set.union(bi1.interface_dirs, bi2.interface_dirs),
        prebuilt_dependencies = set.union(bi1.prebuilt_dependencies, bi2.prebuilt_dependencies),
        cc_dependencies = merge_HaskellCcInfo(bi1.cc_dependencies, bi2.cc_dependencies),
        transitive_cc_dependencies = merge_HaskellCcInfo(bi1.transitive_cc_dependencies, bi2.transitive_cc_dependencies),
    )

def concatBuildInfos(deps):
    buildInfoAcc = HaskellBuildInfo(
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
    )

    for dep in deps:
        if HaskellBuildInfo in dep:
            buildInfoAcc = appendBuildInfos(buildInfoAcc, dep[HaskellBuildInfo])
    return buildInfoAcc

def appendLibraryInfos(ctx, li1, li2, idx):
    exposed_modules_file = ctx.actions.declare_file(
        ctx.attr.name + "-exposed-modules-" + str(idx),
    )
    ctx.actions.run_shell(
        inputs = [li1.exposed_modules_file, li2.exposed_modules_file],
        outputs = [exposed_modules_file],
        command = "cat {file1} <(echo ',') {file2} > {out}".format(
            file1 = li1.exposed_modules_file,
            file2 = li2.exposed_modules_file,
            out = exposed_modules_file,
        ),
    )
    return HaskellLibraryInfo(
        package_id = li1.package_id + [li2.package_id],
        version = li1.version,
        import_dirs = set.union(li1.import_dirs, li2.import_dirs),
        ghc_args = li1.ghc_args + li2.ghc_args,
        header_files = set.union(li1.header_files, li2.header_files),
        boot_files = set.union(li1.boot_files, li2.boot_files),
        source_files = set.union(li1.source_files, li2.source_files),
        exposed_modules_file = exposed_modules_file,
        extra_source_files = li1.extra_source_files.union(li2.extra_source_files),
    )

def concatLibraryInfos(ctx, package_id, deps):
    empty_file = ctx.actions.declare_file("empty")
    ctx.actions.write(
        empty_file,
        "",
    )
    acc = HaskellLibraryInfo(
        package_id = [],
        version = None,
        import_dirs = set.empty(),
        ghc_args = [],
        header_files = set.empty(),
        boot_files = set.empty(),
        source_files = set.empty(),
        exposed_modules_file = empty_file,
        extra_source_files = depset([]),
    )
    idx = 0
    for dep in deps:
        if HaskellLibraryInfo in dep:
            idx += 1
            acc = appendLibraryInfos(ctx, acc, dep[HaskellLibraryInfo], idx)
    return acc

def _haskell_group_impl(ctx):
    hs = haskell_context(ctx)
    deps = ctx.attr.deps

    combinedBuildInfos = concatBuildInfos(deps)
    my_pkg_id = pkg_id.new(ctx.label, None)
    combinedLibraryInfos = concatLibraryInfos(ctx, my_pkg_id, deps)

    build_haskell_repl(
        hs,
        ghci_script = ctx.file._ghci_script,
        ghci_repl_wrapper = ctx.file._ghci_repl_wrapper,
        compiler_flags = [],  # XXX: Forward compiler flags properly
        repl_ghci_args = [],
        output = ctx.outputs.repl,
        package_caches = combinedBuildInfos.package_caches,
        version = combinedLibraryInfos.version,
        build_info = combinedBuildInfos,
        lib_info = combinedLibraryInfos,
    )

    return [combinedBuildInfos, combinedLibraryInfos]

haskell_group = rule(
    _haskell_group_impl,
    attrs = {
        "deps": attr.label_list(
            doc = "List of haskell targets to combine",
        ),
        "_ghci_script": attr.label(
            allow_single_file = True,
            default = Label("@io_tweag_rules_haskell//haskell:assets/ghci_script"),
        ),
        "_ghci_repl_wrapper": attr.label(
            allow_single_file = True,
            default = Label("@io_tweag_rules_haskell//haskell:private/ghci_repl_wrapper.sh"),
        ),
    },
    outputs = {
        "repl": "%{name}@repl",
    },
    toolchains = [
        "@io_tweag_rules_haskell//haskell:toolchain",
    ],
)
"""
Bundle together a group of haskell targets so that they appear as a single target from the outside.
"""
