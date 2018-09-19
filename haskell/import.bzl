"""Importing prebuilt packages into bazel"""

load(":private/context.bzl", "haskell_context")
load(":private/actions/package.bzl", "package")
load(
    ":private/providers.bzl",
    "HaddockInfo",
    "HaskellBuildInfo",
    "HaskellLibraryInfo",
)
load(":private/set.bzl", "set")
load(":private/path_utils.bzl", "ln")
load("@bazel_skylib//:lib.bzl", "paths")

def link_forest(ctx, srcs, basePath = ".", **kwargs):
    """Write a symlink to each file in `srcs` into a destination directory
    defined using the same arguments as `ctx.actions.declare_directory`"""
    local_files = []
    for src in srcs:
        dest = ctx.actions.declare_file(
            paths.join(basePath, src.basename),
            **kwargs
        )
        local_files.append(dest)
        ln(ctx, src, dest)
    return local_files

def copy_all(ctx, srcs, dest):
    """Copy all the files in `srcs` into `dest`"""
    if list(srcs) == []:
        ctx.actions.run_shell(
            command = "mkdir -p {dest}".format(dest = dest.path),
            outputs = [dest],
        )
    else:
        args = ctx.actions.args()
        args.add_all(srcs)
        ctx.actions.run_shell(
            inputs = depset(srcs),
            outputs = [dest],
            mnemonic = "Copy",
            command = "mkdir -p {dest} && cp -L -R \"$@\" {dest}".format(dest = dest.path),
            arguments = [args],
        )

def _haskell_import_impl(ctx):
    hs = haskell_context(ctx)

    package_cache = ctx.actions.declare_file(
        paths.join("package.conf.d", "package.cache"),
    )

    local_package_confs = link_forest(
        ctx = ctx,
        srcs = ctx.attr.package_conf.files,
        sibling = package_cache,
    )

    local_haddock_html = ctx.actions.declare_directory("haddock-html")
    copy_all(
        ctx = ctx,
        srcs = ctx.attr.haddock_html.files,
        dest = local_haddock_html,
    )
    local_haddock_interfaces = link_forest(
        ctx = ctx,
        srcs = ctx.attr.haddock_interfaces.files,
        basePath = "haddock-interfaces",
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
        package_ids = set.from_list([ctx.attr.package_id] + ctx.attr.deps_ids),
        package_confs = set.from_list(local_package_confs),
        package_caches = dependencies_caches,
        static_libraries = [],
        static_libraries_prof = [],
        dynamic_libraries = set.from_list(ctx.attr.dynamic_libraries),
        interface_dirs = set.empty(),
        prebuilt_dependencies = set.empty(),
        external_libraries = {},
        direct_prebuilt_deps = set.empty(),
    )
    html_files = list(ctx.attr.haddock_html.files)
    transitive_html = {ctx.attr.package_id: local_haddock_html}
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
        deps_ids = attr.string_list(),
        deps = attr.label_list(doc = "Haskell dependencies for the package"),
        dynamic_libraries = attr.label_list(),
        version = attr.string(doc = "Package version."),
        deps_caches = attr.label(),
        haddock_interfaces = attr.label(doc = "List of haddock interfaces"),
        haddock_html = attr.label(doc = "List of haddock html dirs"),
        package_conf = attr.label(
            allow_single_file = True,
        ),
    ),
    outputs = {
        "cache": "%{name}-cache",
    },
    toolchains = ["@io_tweag_rules_haskell//haskell:toolchain"],
)
