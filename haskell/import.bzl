"""Importing prebuilt packages into bazel"""

load(":private/context.bzl", "haskell_context")
load(":private/actions/package.bzl", "package")
load(
    ":private/providers.bzl",
    "HaskellBuildInfo",
    "HaskellLibraryInfo",
)
load(":private/set.bzl", "set")
load(":private/path_utils.bzl", "ln")
load("@bazel_skylib//:lib.bzl", "paths")

def _haskell_import_impl(ctx):
    hs = haskell_context(ctx)

    package_cache = ctx.actions.declare_file(
        paths.join(ctx.attr.package_id, "package.conf.d", "package.cache"),
    )

    local_package_confs = []
    for package_conf_file in ctx.attr.package_conf.files:
        local_package_conf = ctx.actions.declare_file(package_conf_file.basename, sibling = package_cache)
        local_package_confs.append(local_package_conf)
        ln(ctx, package_conf_file, local_package_conf)

    package_path = package_cache.dirname + ":"
    recache_args = ctx.actions.args()
    recache_args.add(["-D"] + list(ctx.attr.package_conf.files) + [
        "--target-directory",
        local_package_conf,
    ])
    ctx.actions.run(
        outputs = [package_cache],
        inputs = ctx.attr.package_conf.files + local_package_confs,
        mnemonic = "HaskellCreatePackageCache",
        executable = hs.tools.ghc_pkg,
        arguments = [
            "recache",
            "--package-db",
            package_cache.dirname,
        ],
    )

    libInfo = HaskellLibraryInfo(
        package_id = ctx.attr.package_id,
        version = ctx.attr.version,
        import_dirs = ctx.attr.import_dirs,
        header_files = ctx.attr.header_files,
        boot_files = ctx.attr.boot_files,
        source_files = ctx.attr.source_files,
        extra_source_files = ctx.attr.extra_source_files,
        ghc_args = ctx.attr.ghc_args,
        exposed_modules_file = ctx.attr.exposed_modules_file,
    )
    buildInfo = HaskellBuildInfo(
        package_ids = set.from_list([ctx.attr.package_id]),
        package_confs = set.from_list(local_package_confs),
        package_caches = set.from_list([package_cache]),
        static_libraries = [],
        static_libraries_prof = [],
        dynamic_libraries = set.empty(),
        interface_dirs = set.empty(),
        prebuilt_dependencies = set.empty(),
        external_libraries = {},
        direct_prebuilt_deps = set.empty(),
    )
    return [buildInfo, libInfo]

haskell_import = rule(
    _haskell_import_impl,
    attrs = dict(
        package_id = attr.string(doc = "Workspace unique package identifier"),
        version = attr.string(doc = "Package version."),
        import_dirs = attr.label_list(
            doc = "Import hierarchy roots.",
        ),
        header_files = attr.label_list(doc = "Set of header files."),
        boot_files = attr.label_list(doc = "Set of boot files."),
        source_files = attr.label_list(doc = "Set of files that contain Haskell modules."),
        extra_source_files = attr.label_list(doc = "A depset of non-Haskell source files."),
        ghc_args = attr.string_list(doc = "Arguments that were used to compile the package."),
        exposed_modules_file = attr.label(
            doc = "File containing a list of exposed module names.",
            allow_single_file = True,
        ),
        package_conf = attr.label(
            allow_single_file = True,
        ),
    ),
    toolchains = ["@io_tweag_rules_haskell//haskell:toolchain"],
)
