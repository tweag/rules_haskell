load(
    "@bazel_skylib//:lib.bzl",
    unit = "unittest",
    "asserts",
)
load("//haskell:private/actions/link.bzl", "backup_path")

def link_backup_path_test_impl(ctx):
    env = unit.begin(ctx)
    file_stub = struct(short_path=ctx.attr.filename)
    asserts.equals(
        env,
        expected = ctx.attr.output,
        actual = backup_path(file_stub),
    )
    unit.end(env)

link_backup_path_test = unit.make(
    link_backup_path_test_impl,
    attrs = {
        "filename": attr.string(),
        "output": attr.string(),
    }
)
