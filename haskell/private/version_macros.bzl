load("@bazel_skylib//lib:sets.bzl", "sets")

def generate_version_macros(ctx, pkg_name, version):
    """Generate a version macros header file.

    Args:
        ctx: Rule context. Needs to define a _version_macros executable attribute.
        pkg_name: The package name.
        version: The package version.

    Returns:
        Version macros header File.
    """
    version_macros_file = ctx.actions.declare_file("{}_version_macros.h".format(ctx.attr.name))
    ctx.actions.run_shell(
        tools = [ctx.executable._version_macros],
        outputs = [version_macros_file],
        command = """
        "$1" "$2" "$3" > "$4"
        """,
        arguments = [
            ctx.executable._version_macros.path,
            pkg_name,
            version,
            version_macros_file.path,
        ],
    )
    return version_macros_file

def version_macro_includes(hs_info):
    """Generate a list of version macro header includes.

    Args:
        hs_info: HaskellInfo provider.

    Returns:
        (files, flags):
        files: Set of version macros header files.
        flags: List of C preprocessor flags to include version macros.
    """
    files = hs_info.version_macros
    flags = [
        f
        for include in sets.to_list(files)
        for f in ["-include", include.path]
    ]
    return (files, flags)
