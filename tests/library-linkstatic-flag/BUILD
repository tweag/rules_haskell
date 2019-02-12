load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
)
load("//tests:inline_tests.bzl", "sh_inline_test")
load(":get_library_files.bzl", "get_libraries_as_runfiles")

# test whether `linkstatic` works as expected
package(default_testonly = 1)

# only .a files
haskell_library(
    name = "library-static-only",
    srcs = ["Lib.hs"],
    linkstatic = True,  # <--
    visibility = ["//visibility:public"],
    deps = [
        "//tests/hackage:base",
        "//tests/hackage:bytestring",
    ],
)

# both .a and .so files
haskell_library(
    name = "library-static-and-dynamic",
    srcs = ["Lib.hs"],
    linkstatic = False,  # <--
    visibility = ["//visibility:public"],
    deps = [
        "//tests/hackage:base",
        "//tests/hackage:bytestring",
    ],
)

# extract all libraries from the haskell_library
get_libraries_as_runfiles(
    name = "library-static-only-libraries",
    library = ":library-static-only",
)

get_libraries_as_runfiles(
    name = "library-static-and-dynamic-libraries",
    library = ":library-static-and-dynamic",
)

# sh_test’s `data` doesn’t add stuff to runfiles :(
# sh_library can bundle different targets as runfiles for sh_test
# TODO(Profpatsch): add functionality to sh_inline_test by default?
sh_library(
    name = "bundled-dependency-files-static-only",
    data = [":library-static-only-libraries"],
)

sh_library(
    name = "bundled-dependency-files-static-and-dynamic",
    data = [":library-static-and-dynamic-libraries"],
)

# ensure that linkstatic=True only creates only .a, no .so
sh_inline_test(
    name = "library-linkstatic-flag",
    size = "small",
    # pass the file names as arguments
    args = ["$(rootpaths :library-static-only-libraries)"],
    data = [
        # for rootpaths
        ":library-static-only-libraries",
        # to actually get the files …
        ":bundled-dependency-files-static-only",
    ],
    script = """
set -euo pipefail
for f in "$@"; do
    if ! [[ "$f" =~ .a$ ]]; then
      echo "not a static library: $f"
      exit 1
    fi
done
""",
)

# test whether .so is linked dynamically and .a statically
sh_inline_test(
    name = "test-libraries-static-and-dynamic",
    size = "small",
    # pass the file names as arguments
    args = ["$(rootpaths :library-static-and-dynamic-libraries)"],
    data = [
        # for rootpaths
        ":library-static-and-dynamic-libraries",
        # to actually get the files …
        ":bundled-dependency-files-static-and-dynamic",
    ],
    script = """
set -euo pipefail
is_dynamic () {
    # taken from https://github.com/NixOS/nixpkgs/blob/0b3f50f844e2a6b507b18d7c5259bb850b382f87/pkgs/build-support/setup-hooks/auto-patchelf.sh#L167-L170
    readelf -l -- "$1" | grep -q "^ *INTERP\\>"
}

for f in "$@"; do
    if [[ "$f" =~ .a$ ]] && is_dynamic "$f"; then
        echo "should be a static executable: $f"
        exit 1
    fi
    if [[ "$f" =~ .so$ ]] && ! is_dynamic "$f"; then
        echo "should be a dynamic executable: $f"
        exit 1
    fi
done
""",
)
