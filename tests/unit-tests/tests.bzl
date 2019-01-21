load(
    "@bazel_skylib//lib:unittest.bzl",
    "asserts",
    unit = "unittest",
)
load("//haskell:private/actions/link.bzl", "backup_path")
load("//haskell:private/list.bzl", "list")

def link_backup_path_test_impl(ctx):
    env = unit.begin(ctx)
    file_stub = struct(short_path = ctx.attr.filename)
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
    },
)

def compare_x(el):
    return el.x

def dedup_on_test_impl(ctx):
    env = unit.begin(ctx)
    asserts.equals(
        env,
        expected = [],
        actual = list.dedup_on(compare_x, []),
    )
    asserts.equals(
        env,
        expected = [struct(x = 3)],
        actual = list.dedup_on(
            compare_x,
            [struct(x = 3), struct(x = 3), struct(x = 3)],
        ),
    )
    asserts.equals(
        env,
        expected = [struct(x = 3), struct(x = 4), struct(x = 5)],
        actual = list.dedup_on(
            compare_x,
            [struct(x = 3), struct(x = 4), struct(x = 3), struct(x = 5), struct(x = 3)],
        ),
    )
    unit.end(env)

dedup_on_test = unit.make(dedup_on_test_impl)
