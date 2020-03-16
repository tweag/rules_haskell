def _dynamic_libraries_impl(ctx):
    outputs = []
    solib_names = []
    for target in ctx.attr.srcs:
        cc_info = target[CcInfo]
        for library_to_link in cc_info.linking_context.libraries_to_link.to_list():
            library = library_to_link.resolved_symlink_dynamic_library
            if not library or library.basename.find(ctx.attr.filter) == -1:
                continue
            outputs.append(library)
            if library_to_link.dynamic_library:
                solib_names.append(library_to_link.dynamic_library.short_path)
    if ctx.attr.solib_names:
        ctx.actions.write(
            ctx.outputs.solib_names,
            "\n".join(solib_names),
        )
    return [DefaultInfo(
        files = depset(outputs),
        runfiles = ctx.runfiles(files = outputs),
    )]

dynamic_libraries = rule(
    _dynamic_libraries_impl,
    attrs = {
        "filter": attr.string(
            doc = "Skip libraries that do not contain this string in their name.",
        ),
        "srcs": attr.label_list(
            doc = "Extract dynamic libraries from these targets",
            providers = [CcInfo],
        ),
        "solib_names": attr.output(
            doc = "Write the `_solib_<cpu>` paths of the dynamic libraries to this file.",
        ),
    },
    doc = "Extract the dynamic libraries from cc_library targets.",
)
