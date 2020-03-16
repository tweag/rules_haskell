load(
    "//haskell:providers.bzl",
    "HaskellInfo",
    "HaskellLibraryInfo",
)
load(":private/set.bzl", "set")

def gather_dep_info(ctx, deps):
    """Collapse dependencies into a single `HaskellInfo`.

    Args:
      ctx: Rule context.
      deps: deps attribute.

    Returns:
      HaskellInfo: Unified information about all dependencies.
    """

    package_databases = depset(transitive = [
        dep[HaskellInfo].package_databases
        for dep in deps
        if HaskellInfo in dep
    ])
    static_libraries = depset(transitive = [
        dep[HaskellInfo].static_libraries
        for dep in deps
        if HaskellInfo in dep
    ])
    dynamic_libraries = depset(transitive = [
        dep[HaskellInfo].dynamic_libraries
        for dep in deps
        if HaskellInfo in dep
    ])
    interface_dirs = depset(transitive = [
        dep[HaskellInfo].interface_dirs
        for dep in deps
        if HaskellInfo in dep
    ])

    source_files = depset(transitive = [
        dep[HaskellInfo].source_files
        for dep in deps
        if HaskellInfo in dep
    ])

    import_dirs = set.empty()
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
        version_macros = set.empty(),
        static_libraries = static_libraries,
        dynamic_libraries = dynamic_libraries,
        interface_dirs = interface_dirs,
        source_files = source_files,
        import_dirs = import_dirs,
        extra_source_files = extra_source_files,
        compile_flags = compile_flags,
    )

    for dep in deps:
        if HaskellInfo in dep:
            binfo = dep[HaskellInfo]
            if HaskellLibraryInfo not in dep:
                fail("Target {0} cannot depend on binary".format(ctx.attr.name))
            acc = HaskellInfo(
                package_databases = acc.package_databases,
                version_macros = set.mutable_union(acc.version_macros, binfo.version_macros),
                static_libraries = depset(transitive = [acc.static_libraries, binfo.static_libraries]),
                dynamic_libraries = acc.dynamic_libraries,
                interface_dirs = acc.interface_dirs,
                import_dirs = import_dirs,
                compile_flags = compile_flags,
                extra_source_files = extra_source_files,
                source_files = source_files,
            )
        elif CcInfo in dep and HaskellInfo not in dep:
            # The final link of a binary must include all static libraries we
            # depend on, including transitives ones. Theses libs are provided
            # in the `CcInfo` provider.
            acc = HaskellInfo(
                package_databases = acc.package_databases,
                version_macros = acc.version_macros,
                import_dirs = acc.import_dirs,
                source_files = acc.source_files,
                compile_flags = acc.compile_flags,
                static_libraries = acc.static_libraries,
                dynamic_libraries = acc.dynamic_libraries,
                extra_source_files = acc.extra_source_files,
                interface_dirs = acc.interface_dirs,
            )

    return acc
