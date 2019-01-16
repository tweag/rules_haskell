load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
    "haskell_test",
)
load("//tests:sh_inline_test.bzl", "sh_inline_test")
load(":get_library_files.bzl", "get_libraries_as_runfiles")

# test whether `linkstatic` works as expected
package(default_testonly = 1)

cc_library(
    name = "c-lib",
    srcs = ["c-lib.c"],
)

haskell_library(
    name = "CLib",
    srcs = ["CLib.hsc"],
    deps = [
        ":c-lib",
        "@hackage//:base",
    ],
)

haskell_library(
    name = "HsLib",
    srcs = ["HsLib.hs"],
    deps = [
        "@hackage//:base",
    ],
)

haskell_test(
    name = "binary-static",
    srcs = ["Main.hs"],
    linkstatic = True,
    deps = [
        ":CLib",
        ":HsLib",
        "@hackage//:base",
    ],
)

haskell_test(
    name = "binary-dynamic",
    srcs = ["Main.hs"],
    linkstatic = False,
    deps = [
        ":CLib",
        ":HsLib",
        "@hackage//:base",
    ],
)

# extract all libraries from the Haskell binary
get_libraries_as_runfiles(
    name = "binary-static-libraries",
    binary = ":binary-static",
)

get_libraries_as_runfiles(
    name = "binary-dynamic-libraries",
    binary = ":binary-dynamic",
)

# sh_test’s `data` doesn’t add stuff to runfiles :(
# sh_library can bundle different targets as runfiles for sh_test
# TODO(Profpatsch): add functionality to sh_inline_test by default?
sh_library(
    name = "bundled-dependency-files-static",
    data = [":binary-static-libraries"],
)

sh_library(
    name = "bundled-dependency-files-dynamic",
    data = [":binary-dynamic-libraries"],
)

# The dynamic libraries that the binary references
genrule(
    name = "binary-static-ldd",
    srcs = [":binary-static"],
    outs = ["binary-static-ldd.txt"],
    cmd = """
    objdump -x $(SRCS) | sed -ne 's/^  NEEDED\s*//p' > $(OUTS)
    """,
)

genrule(
    name = "binary-dynamic-ldd",
    srcs = [":binary-dynamic"],
    outs = ["binary-dynamic-ldd.txt"],
    cmd = """
    objdump -x $(SRCS) | sed -ne 's/^  NEEDED\s*//p' > $(OUTS)
    """,
)

# ensure that linkstatic=True only links to static library targets.
sh_inline_test(
    name = "test-binary-static-only",
    size = "small",
    args = [
        "$(rootpath :binary-static-ldd)",
        # pass the file names as arguments
        "$(rootpaths :binary-static-libraries)",
    ],
    data = [
        # for rootpaths
        ":binary-static-ldd",
        ":binary-static-libraries",
        # to actually get the files …
        ":bundled-dependency-files-static",
    ],
    script = """
set -euo pipefail
ldd=$1
shift 1
for f in "$@"; do
    if [[ "$f" =~ .so$ || "$f" =~ .dylib$ ]]; then
        if grep -q "$(basename "$f")" "$ldd"; then
            echo "static binary should not depend on dynamic library $f" >&2
            exit 1
        fi
    fi
done
""",
)

# ensure that linkstatic=False links to dynamic library targets.
sh_inline_test(
    name = "test-binary-dynamic-only",
    size = "small",
    args = [
        "$(rootpath :binary-dynamic-ldd)",
        # pass the file names as arguments
        "$(rootpaths :binary-dynamic-libraries)",
    ],
    data = [
        # for rootpaths
        ":binary-dynamic-ldd",
        ":binary-dynamic-libraries",
        # to actually get the files …
        ":bundled-dependency-files-dynamic",
    ],
    script = """
set -euo pipefail
ldd=$1
shift 1
for f in "$@"; do
    if [[ "$f" =~ .so$ || "$f" =~ .dylib$ ]]; then
        if ! grep -q "$(basename "$f")" "$ldd"; then
            echo "dynamic binary should depend on dynamic library $f" >&2
            exit 1
        fi
    fi
done
""",
)

test_suite(
    name = "binary-linkstatic-flag",
    tests = [
        ":test-binary-dynamic-only",
        ":test-binary-static-only",
    ],
    visibility = ["//visibility:public"],
)
