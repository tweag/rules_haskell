load("@bazel_skylib//lib:sets.bzl", "sets")
load(
    "//haskell:providers.bzl",
    "HaskellInfo",
    "HaskellLibraryInfo",
)
load(":private/set.bzl", "set")

def gather_dep_info(name, deps):
    """Collapse dependencies into a single `HaskellInfo`.

    Args:
      name: Rule name
      deps: deps attribute.

    Returns:
      HaskellInfo: Unified information about all dependencies.
    """

    package_databases = depset(transitive = [
        dep[HaskellInfo].package_databases
        for dep in deps
        if HaskellInfo in dep
    ])
    empty_lib_package_databases = depset(transitive = [
        dep[HaskellInfo].empty_lib_package_databases
        for dep in deps
        if HaskellInfo in dep
    ])
    hs_libraries = depset(transitive = [
        dep[HaskellInfo].hs_libraries
        for dep in deps
        if HaskellInfo in dep
    ])
    deps_hs_libraries = depset(transitive = [
        dep[HaskellInfo].deps_hs_libraries
        for dep in deps
        if HaskellInfo in dep
    ])
    interface_dirs = depset(transitive = [
        dep[HaskellInfo].interface_dirs
        for dep in deps
        if HaskellInfo in dep
    ])
    deps_interface_dirs = depset(transitive = [
        dep[HaskellInfo].deps_interface_dirs
        for dep in deps
        if HaskellInfo in dep
    ])

    source_files = depset(transitive = [
        dep[HaskellInfo].source_files
        for dep in deps
        if HaskellInfo in dep
    ])

    boot_files = depset(transitive = [
        dep[HaskellInfo].boot_files
        for dep in deps
        if HaskellInfo in dep
    ])

    import_dirs = sets.make()
    for dep in deps:
        if HaskellInfo in dep:
            import_dirs = set.mutable_union(import_dirs, dep[HaskellInfo].import_dirs)

    extra_source_files = depset(transitive = [
        dep[HaskellInfo].extra_source_files
        for dep in deps
        if HaskellInfo in dep
    ])

    compile_flags = []
    for dep in deps:
        if HaskellInfo in dep:
            compile_flags.extend(dep[HaskellInfo].compile_flags)

    acc = HaskellInfo(
        package_databases = package_databases,
        empty_lib_package_databases = empty_lib_package_databases,
        version_macros = sets.make(),
        hs_libraries = hs_libraries,
        deps_hs_libraries = deps_hs_libraries,
        interface_dirs = interface_dirs,
        deps_interface_dirs = deps_interface_dirs,
        source_files = source_files,
        boot_files = boot_files,
        import_dirs = import_dirs,
        extra_source_files = extra_source_files,
        compile_flags = compile_flags,
        user_compile_flags = [],
        user_repl_flags = [],
    )

    for dep in deps:
        if HaskellInfo in dep:
            binfo = dep[HaskellInfo]
            if HaskellLibraryInfo not in dep:
                fail("Target {0} cannot depend on binary".format(name))
            acc = HaskellInfo(
                package_databases = acc.package_databases,
                empty_lib_package_databases = acc.empty_lib_package_databases,
                version_macros = set.mutable_union(acc.version_macros, binfo.version_macros),
                hs_libraries = depset(transitive = [acc.hs_libraries, binfo.hs_libraries]),
                deps_hs_libraries = depset(transitive = [acc.deps_hs_libraries, binfo.deps_hs_libraries]),
                interface_dirs = acc.interface_dirs,
                deps_interface_dirs = acc.deps_interface_dirs,
                import_dirs = import_dirs,
                compile_flags = compile_flags,
                extra_source_files = extra_source_files,
                source_files = source_files,
                boot_files = boot_files,
                user_compile_flags = [],
                user_repl_flags = [],
            )
        elif CcInfo in dep and HaskellInfo not in dep:
            # The final link of a binary must include all static libraries we
            # depend on, including transitives ones. Theses libs are provided
            # in the `CcInfo` provider.
            acc = HaskellInfo(
                package_databases = acc.package_databases,
                empty_lib_package_databases = acc.empty_lib_package_databases,
                version_macros = acc.version_macros,
                import_dirs = acc.import_dirs,
                source_files = acc.source_files,
                boot_files = acc.boot_files,
                compile_flags = acc.compile_flags,
                hs_libraries = acc.hs_libraries,
                deps_hs_libraries = acc.deps_hs_libraries,
                extra_source_files = acc.extra_source_files,
                interface_dirs = acc.interface_dirs,
                deps_interface_dirs = acc.deps_interface_dirs,
                user_compile_flags = [],
                user_repl_flags = [],
            )

    return acc
