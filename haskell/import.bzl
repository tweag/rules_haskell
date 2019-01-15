"""Importing prebuilt packages into bazel"""

load("@bazel_skylib//lib:paths.bzl", "paths")
load(
    "@io_tweag_rules_haskell//haskell:private/providers.bzl",
    "HaddockInfo",
    "HaskellBuildInfo",
    "HaskellLibraryInfo",
)
load(":private/context.bzl", "haskell_context")
load(":private/path_utils.bzl", "copy_all", "link_forest", "ln")
load(":private/set.bzl", "set")

def _haskell_import_impl(ctx):
    hs = haskell_context(ctx)

    package_cache = ctx.actions.declare_file(
        paths.join("package.conf.d", "package.cache"),
    )

    local_package_confs = link_forest(
        ctx = ctx,
        srcs = ctx.attr.package_confs.files,
        sibling = package_cache,
    )

    local_haddock_html = ctx.actions.declare_directory("haddock-html")
    copy_all(
        ctx = ctx,
        srcs = ctx.attr.haddock_html.files,
        dest = local_haddock_html,
    )

    ctx.actions.run(
        outputs = [package_cache],
        inputs = local_package_confs,
        mnemonic = "HaskellCreatePackageCache",
        executable = hs.tools.ghc_pkg,
        arguments = [
            "recache",
            "--package-db",
            package_cache.dirname,
        ],
    )
    ln(ctx, package_cache, ctx.outputs.cache)

    dependencies_caches = set.singleton(package_cache)
    for dep in ctx.attr.deps:
        if HaskellBuildInfo in dep:
            set.mutable_union(dependencies_caches, dep[HaskellBuildInfo].package_caches)

    deps_ids = [
        dep[HaskellLibraryInfo].package_id
        for dep in ctx.attr.deps
        if HaskellLibraryInfo in dep
    ]

    libInfo = HaskellLibraryInfo(
        package_id = ctx.attr.package_id,
        version = ctx.attr.version,
        import_dirs = [],
        header_files = set.empty(),
        boot_files = set.empty(),
        source_files = set.empty(),
        extra_source_files = set.empty(),
        ghc_args = [],
    )
    buildInfo = HaskellBuildInfo(
        package_ids = set.from_list([ctx.attr.package_id] + deps_ids),
        package_confs = set.from_list(local_package_confs),
        package_caches = dependencies_caches,
        static_libraries = [],
        static_libraries_prof = [],
        dynamic_libraries = set.empty(),
        interface_dirs = set.empty(),
        prebuilt_dependencies = set.empty(),
        external_libraries = set.empty(),
        direct_prebuilt_deps = set.empty(),
        extra_libraries = set.empty(),
    )
    html_files = list(ctx.attr.haddock_html.files)
    transitive_html = {ctx.attr.package_id: local_haddock_html} if html_files != [] else {}
    interface_files = list(ctx.attr.haddock_interfaces.files)
    transitive_haddocks = {ctx.attr.package_id: interface_files[0]} if interface_files != [] else {}

    haddockInfo = HaddockInfo(
        package_id = ctx.attr.package_id,
        transitive_html = transitive_html,
        transitive_haddocks = transitive_haddocks,
    )
    return [buildInfo, libInfo, haddockInfo]

haskell_import = rule(
    _haskell_import_impl,
    attrs = dict(
        package_id = attr.string(doc = "Workspace unique package identifier"),
        deps = attr.label_list(doc = "Haskell dependencies for the package"),
        version = attr.string(doc = "Package version."),
        haddock_interfaces = attr.label(doc = "List of haddock interfaces"),
        haddock_html = attr.label(doc = "List of haddock html dirs"),
        package_confs = attr.label(doc = "List of ghc-pkg package.conf files"),
    ),
    outputs = {
        "cache": "%{name}-cache",
    },
    toolchains = ["@io_tweag_rules_haskell//haskell:toolchain"],
)
