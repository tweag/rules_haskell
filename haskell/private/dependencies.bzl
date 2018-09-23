load(":private/path_utils.bzl", "ln")
load(
    ":private/providers.bzl",
    "CcSkylarkApiProviderHacked",
    "HaskellBinaryInfo",
    "HaskellBuildInfo",
    "HaskellLibraryInfo",
    "HaskellPrebuiltPackageInfo",
)
load(":private/set.bzl", "set")
load("@bazel_skylib//:lib.bzl", "dicts", "paths")

def _is_shared_library(f):
    """Check if the given File is a shared library.

    Args:
      f: The File to check.

    Returns:
      Bool: True if the given file `f` is a shared library, False otherwise.
    """
    return f.extension in ["so", "dylib"] or f.basename.find(".so.") != -1

def _is_static_library(f):
    """Check if the given File is a static library.

    Args:
      f: The File to check.

    Returns:
      Bool: True if the given file `f` is a static library, False otherwise.
    """
    return f.extension in ["a"]

def _mangle_solib(ctx, label, solib, preserve_name):
    """Create a symlink to a dynamic library, with a longer name.

    The built-in cc_* rules don't link against a shared library
    directly. They link against a symlink whose name is guaranteed to be
    unique across the entire workspace. This disambiguates dynamic
    libraries with the same soname. This process is called "mangling".
    The built-in rules don't expose mangling functionality directly (see
    https://github.com/bazelbuild/bazel/issues/4581). But this function
    emulates the built-in dynamic library mangling.

    Args:
      ctx: Rule context.
      label: the label to use as a qualifier for the dynamic library name.
      solib: the dynamic library.
      preserve_name: Bool, whether given `solib` should be returned unchanged.

    Returns:
      File: the created symlink or the original solib.
    """

    if preserve_name:
        return solib

    components = [c for c in [label.workspace_root, label.package, label.name] if c]
    qualifier = "/".join(components).replace("_", "_U").replace("/", "_S")
    qualsolib = ctx.actions.declare_file("lib" + qualifier + "_" + solib.basename)

    ln(ctx, solib, qualsolib)

    return qualsolib

def gather_dep_info(ctx):
    """Collapse dependencies into a single `HaskellBuildInfo`.

    Note that the field `prebuilt_dependencies` also includes
    prebuilt_dependencies of current target.

    Args:
      ctx: Rule context.

    Returns:
      HaskellBuildInfo: Unified information about all dependencies.
    """

    acc = HaskellBuildInfo(
        package_ids = set.empty(),
        package_confs = set.empty(),
        package_caches = set.empty(),
        static_libraries = [],
        static_libraries_prof = [],
        dynamic_libraries = set.empty(),
        interface_dirs = set.empty(),
        prebuilt_dependencies = set.from_list(ctx.attr.prebuilt_dependencies),
        external_libraries = {},
        direct_prebuilt_deps = set.from_list(ctx.attr.prebuilt_dependencies),
    )

    for dep in ctx.attr.deps:
        if HaskellBuildInfo in dep:
            binfo = dep[HaskellBuildInfo]
            package_ids = acc.package_ids
            if HaskellBinaryInfo in dep:
                fail("Target {0} cannot depend on binary".format(ctx.attr.name))
            if HaskellLibraryInfo in dep:
                set.mutable_insert(package_ids, dep[HaskellLibraryInfo].package_id)
            acc = HaskellBuildInfo(
                package_ids = package_ids,
                package_confs = set.mutable_union(acc.package_confs, binfo.package_confs),
                package_caches = set.mutable_union(acc.package_caches, binfo.package_caches),
                static_libraries = acc.static_libraries + binfo.static_libraries,
                static_libraries_prof = acc.static_libraries_prof + binfo.static_libraries_prof,
                dynamic_libraries = set.mutable_union(acc.dynamic_libraries, binfo.dynamic_libraries),
                interface_dirs = set.mutable_union(acc.interface_dirs, binfo.interface_dirs),
                prebuilt_dependencies = set.mutable_union(acc.prebuilt_dependencies, binfo.prebuilt_dependencies),
                external_libraries = dicts.add(acc.external_libraries, binfo.external_libraries),
                direct_prebuilt_deps = acc.direct_prebuilt_deps,
            )
        elif HaskellPrebuiltPackageInfo in dep:
            pkg = dep[HaskellPrebuiltPackageInfo].package
            acc = HaskellBuildInfo(
                package_ids = acc.package_ids,
                package_confs = acc.package_confs,
                package_caches = acc.package_caches,
                static_libraries = acc.static_libraries,
                static_libraries_prof = acc.static_libraries_prof,
                dynamic_libraries = acc.dynamic_libraries,
                interface_dirs = acc.interface_dirs,
                prebuilt_dependencies = set.mutable_insert(acc.prebuilt_dependencies, pkg),
                external_libraries = acc.external_libraries,
                direct_prebuilt_deps = set.mutable_insert(acc.direct_prebuilt_deps, pkg),
            )
        else:
            transitive_deps = {}

            # Transitives static dependencies
            if hasattr(dep, "cc"):
                transitive_deps = {
                    name: _mangle_solib(ctx, dep.label, name, CcSkylarkApiProviderHacked in dep)
                    for name in dep.cc.libs.to_list()
                    if _is_static_library(name)
                }

            # If not a Haskell dependency, pass it through as-is to the
            # linking phase.
            acc = HaskellBuildInfo(
                package_ids = acc.package_ids,
                package_confs = acc.package_confs,
                package_caches = acc.package_caches,
                static_libraries = acc.static_libraries,
                static_libraries_prof = acc.static_libraries_prof,
                dynamic_libraries = acc.dynamic_libraries,
                interface_dirs = acc.interface_dirs,
                prebuilt_dependencies = acc.prebuilt_dependencies,
                external_libraries = dicts.add(
                    acc.external_libraries,
                    {
                        f:
                        # If the provider is CcSkylarkApiProviderHacked, then the .so
                        # files come from haskell_cc_import.
                        _mangle_solib(ctx, dep.label, f, CcSkylarkApiProviderHacked in dep)
                        for f in dep.files.to_list()
                        if _is_shared_library(f)
                    },
                    transitive_deps,
                ),
                direct_prebuilt_deps = acc.direct_prebuilt_deps,
            )

    return acc
