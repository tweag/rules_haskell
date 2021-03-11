load("@bazel_skylib//lib:paths.bzl", "paths")

def _add_data_impl(ctx):
    (_, extension) = paths.split_extension(ctx.executable.executable.path)
    executable = ctx.actions.declare_file(
        ctx.label.name + extension,
    )
    ctx.actions.symlink(
        output = executable,
        target_file = ctx.executable.executable,
        is_executable = True,
    )

    runfiles = ctx.runfiles(files = [executable, ctx.executable.executable] + ctx.files.data)
    runfiles = runfiles.merge(ctx.attr.executable[DefaultInfo].default_runfiles)
    for data_dep in ctx.attr.data:
        runfiles = runfiles.merge(data_dep[DefaultInfo].default_runfiles)

    return [DefaultInfo(
        executable = executable,
        files = depset(direct = [executable]),
        runfiles = runfiles,
    )]

add_data = rule(
    _add_data_impl,
    attrs = {
        "executable": attr.label(
            executable = True,
            cfg = "target",
            doc = "Create a symlink to this executable",
        ),
        "data": attr.label_list(
            allow_files = True,
            doc = "Add these data files to the executable's runfiles",
        ),
    },
    executable = True,
    doc = "Creates a new target for the given executable with additional runfiles.",
)
