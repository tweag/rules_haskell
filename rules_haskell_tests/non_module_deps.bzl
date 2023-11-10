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

def repositories(*, bzlmod):
    # Some helpers for platform-dependent configuration
    os_info(name = "os_info")

    # module rules_bazel_integration_test requires bazel >= 6.1.0
    http_archive(
        name = "rules_bazel_integration_test",
        sha256 = "6e65d497c68f5794349bfa004369e144063686ce1ebd0227717cd23285be45ef",
        urls = [
            "https://github.com/bazel-contrib/rules_bazel_integration_test/releases/download/v0.20.0/rules_bazel_integration_test.v0.20.0.tar.gz",
        ],
    )

    http_archive(
        name = "cgrindel_bazel_starlib",
        sha256 = "9090280a9cff7322e7c22062506b3273a2e880ca464e520b5c77fdfbed4e8805",
        urls = [
            "https://github.com/cgrindel/bazel-starlib/releases/download/v0.18.1/bazel-starlib.v0.18.1.tar.gz",
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
        sha256 = "42968f9134ba2c75c03bb271bd7bb062afb7da449f9b913c96e5be4ce890030a",
        # fix runner.bash.template always returning success, format MODULE.bazel and WORKSPACE.bzlmod too
        patches = ["@rules_haskell//buildifier:buildifier_test-workspace.patch"],
        patch_args = ["-p1"],
        strip_prefix = "buildtools-6.3.3",
        urls = ["https://github.com/bazelbuild/buildtools/archive/v6.3.3.tar.gz"],
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
    # Needed because XCode 12.0 Clang errors by default.
    # See https://developer.apple.com/documentation/xcode-release-notes/xcode-12-release-notes.
    copts = ["-Wno-error=implicit-function-declaration"],
    # Cabal packages depending on dynamic C libraries fail on MacOS
    # due to `-rpath` flags being forwarded indiscriminately.
    # See https://github.com/tweag/rules_haskell/issues/1317
    linkstatic = is_darwin,
)
""",
        sha256 = "b5b06d60ce49c8ba700e0ba517fa07de80b5d4628a037f4be8ad16955be7a7c0",
        strip_prefix = "zlib-1.3",
        urls = ["https://github.com/madler/zlib/archive/v1.3.tar.gz"],
    )

def _non_module_deps_impl(_ctx):
    repositories(bzlmod = True)

non_module_deps = module_extension(
    implementation = _non_module_deps_impl,
)
