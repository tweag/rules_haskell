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

# test comes_from_haskell_cc_import

create_rpath_entry_test(
    name = "rpath_entry_comes_from_haskell_cc_import",
    # full path is up through the bazel-out directory
    binary_path = "bazel-out/k8-fastbuild/bin/foo/baz/a.so",
    binary_short_path = "foo/baz/a.so",
    # it’s been imported with haskell_cc_import
    comes_from_haskell_cc_import = True,
    # the full path contains `external` as first element
    dependency_path = "external/bar/lib/b.so",
    # the short path is that of a normal external dependency
    dependency_short_path = "../bar/lib/b.so",
    # the output goes up all the way to the package root
    # and then down .path of our external dependency
    output = "../../../../../external/bar/lib",
)

dedup_on_test(
    name = "dedup_on_test",
)
