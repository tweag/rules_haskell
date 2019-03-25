load(
    ":tests.bzl",
    "create_rpath_entry_test",
    "dedup_on_test",
    "parent_dir_path_test",
)

parent_dir_path_test(
    name = "parent_dir_just_file",
    filename = "foo",
    output = ["."],
)

parent_dir_path_test(
    name = "parent_dir",
    filename = "foo/",
    output = ["foo"],
)

parent_dir_path_test(
    name = "parent_dir_file",
    filename = "foo/bar",
    output = ["foo"],
)

parent_dir_path_test(
    name = "parent_dir_file_dots",
    filename = "foo/../bar",
    output = [
        "foo",
        "..",
    ],
)

parent_dir_path_test(
    name = "parent_dir_rooted",
    filename = "/foo/bar",
    output = [
        "",
        "foo",
    ],
)

create_rpath_entry_test(
    name = "rpath_entry_simple",
    binary_short_path = "foo/a.so",
    dependency_short_path = "bar/b.so",
    output = "../bar",
)

# checks that a binary in //:bin works properly
create_rpath_entry_test(
    name = "rpath_entry_binary_root",
    binary_short_path = "bin",
    dependency_short_path = "xyz/b.so",
    output = "xyz",
)

# same for dependency
create_rpath_entry_test(
    name = "rpath_entry_dep_root",
    binary_short_path = "lib/bin",
    dependency_short_path = "b.so",
    output = "..",
)

create_rpath_entry_test(
    name = "rpath_entry_simple_filename",
    binary_short_path = "foo/a.so",
    dependency_short_path = "bar/b.so",
    keep_filename = True,
    output = "../bar/b.so",
)

create_rpath_entry_test(
    name = "rpath_entry_prefix",
    binary_short_path = "foo/a.so",
    dependency_short_path = "bar/b.so",
    output = "$ORIGIN/../bar",
    prefix = "$ORIGIN",
)

# if the short-paths have leading dots, they are in `external`

create_rpath_entry_test(
    name = "rpath_entry_binary_leading_dots_dep",
    # non-external
    binary_short_path = "foo/a.so",
    # external dep
    dependency_short_path = "../bar/b.so",
    output = "../external/bar",
)

create_rpath_entry_test(
    name = "rpath_entry_binary_leading_dots_bin",
    # external dep
    binary_short_path = "../foo/a.so",
    # non-external
    dependency_short_path = "bar/b.so",
    # back through `external`
    output = "../../bar",
)

create_rpath_entry_test(
    name = "rpath_entry_binary_leading_dots_both",
    # external dep
    binary_short_path = "../foo/a.so",
    # external dep
    dependency_short_path = "../bar/b.so",
    # stay in `external`
    output = "../bar",
)

# we have no idea how to handle internal dots, should they arise
# create_rpath_entry_test(
#     name = "rpath_entry_binary_internal_dots",
#     binary_short_path = "foo/../../a.so",
#     dependency_short_path = "../bar/../b.so",
#     # but that doesn’t change anything for the runpath
#     output = "../bar",
# )

dedup_on_test(
    name = "dedup_on_test",
)
