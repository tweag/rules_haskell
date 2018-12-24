load(":tests.bzl", "link_backup_path_test")

link_backup_path_test(
    name = "backup_path_file",
    filename = "foo",
    output = ".",
)

link_backup_path_test(
    name = "backup_path_dir_file",
    filename = "foo/bar",
    output = "..",
)

link_backup_path_test(
    name = "backup_path_dir_dir_file",
    filename = "foo/bar/baz",
    output = "../..",
)
