""" External repositories for the CI that need to be shared between WORKSPACE and MODULE.bazel files """

load("@rules_haskell//tools:os_info.bzl", "os_info")
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load(
    "//tests/external-haskell-repository:workspace_dummy.bzl",
    "haskell_package_repository_dummy",
)

# Replaces local_repository in bzlmod
# See https://groups.google.com/g/bazel-discuss/c/xpsg3mWQPZg
def _starlarkified_local_repository_impl(repository_ctx):
    relative_path = repository_ctx.attr.path
    workspace_root = repository_ctx.path(Label("@//:MODULE.bazel")).dirname
    absolute_path = workspace_root
    for segment in relative_path.split("/"):
        absolute_path = absolute_path.get_child(segment)
    repository_ctx.symlink(absolute_path, ".")

starlarkified_local_repository = repository_rule(
    implementation = _starlarkified_local_repository_impl,
    attrs = {
        "path": attr.string(mandatory = True),
    },
)

def repositories(*, bzlmod):  # @unused
    # Some helpers for platform-dependent configuration
    os_info(name = "os_info")

    # module rules_bazel_integration_test requires bazel >= 6.1.0
    http_archive(
        name = "rules_bazel_integration_test",
        sha256 = "fe43a0ef76323813c912b7256a5f01f87f2697528b107627b70da58c50b1988a",
        urls = [
            "https://github.com/bazel-contrib/rules_bazel_integration_test/releases/download/v0.23.0/rules_bazel_integration_test.v0.23.0.tar.gz",
        ],
    )

    http_archive(
        name = "cgrindel_bazel_starlib",
        sha256 = "00b084e895146d2dc8c76437dd5f91a7203c7b46bb4edd1896d018b8795bc927",
        urls = [
            "https://github.com/cgrindel/bazel-starlib/releases/download/v0.20.2/bazel-starlib.v0.20.2.tar.gz",
        ],
    )

    # c2hs rule in its own repository
    starlarkified_local_repository(
        name = "c2hs_repo",
        path = "tests/c2hs/repo",
    )

    # haskell_library rule in its own repository
    starlarkified_local_repository(
        name = "library_repo",
        path = "tests/library-external-workspace/repo",
    )

    # dummy repo for the external haskell repo test
    haskell_package_repository_dummy(
        name = "haskell_package_repository_dummy",
    )

    # no modules are provided at the moment for buildifier
    http_archive(
        name = "com_github_bazelbuild_buildtools",
        sha256 = "60a9025072ae237f325d0e7b661e1685f34922c29883888c2d06f5789462b939",
        strip_prefix = "buildtools-7.1.1",
        urls = ["https://github.com/bazelbuild/buildtools/archive/v7.1.1.tar.gz"],
    )

    http_archive(
        name = "zlib.hs",
        build_file_content = """
load("@os_info//:os_info.bzl", "is_darwin")
load("@rules_cc//cc:defs.bzl", "cc_library")
cc_library(
    name = "zlib",
    # Import `:z` as `srcs` to enforce the library name `libz.so`. Otherwise,
    # Bazel would mangle the library name and e.g. Cabal wouldn't recognize it.
    srcs = [":z"],
    hdrs = glob(["*.h"]),
    includes = ["."],
    linkstatic = 1,
    visibility = ["//visibility:public"],
)
cc_library(
    name = "z",
    srcs = glob(["*.c"]),
    hdrs = glob(["*.h"]),
    copts = select({
        "@bazel_tools//src/conditions:windows": [],
        # Needed to avoid "call to undeclared function" errors [-Wimplicit-function-declaration]
        "//conditions:default": ["-DZ_HAVE_UNISTD_H"],
    }),
    # Cabal packages depending on dynamic C libraries fail on MacOS
    # due to `-rpath` flags being forwarded indiscriminately.
    # See https://github.com/tweag/rules_haskell/issues/1317
    linkstatic = is_darwin,
)
""",
        sha256 = "9a93b2b7dfdac77ceba5a558a580e74667dd6fede4585b91eefb60f03b72df23",
        strip_prefix = "zlib-1.3.1",
        urls = ["https://github.com/madler/zlib/releases/download/v1.3.1/zlib-1.3.1.tar.gz"],
    )

def _non_module_deps_impl(_ctx):
    repositories(bzlmod = True)

non_module_deps = module_extension(
    implementation = _non_module_deps_impl,
)
