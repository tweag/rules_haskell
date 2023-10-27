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
        sha256 = "567ef30550a90c0dc22a9076743b2b2715b4d7148b4d573bdcc61aaa5d0177b3",
        urls = [
            "https://github.com/bazel-contrib/rules_bazel_integration_test/releases/download/v0.18.0/rules_bazel_integration_test.v0.18.0.tar.gz",
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
        sha256 = "977a0bd4593c8d4c8f45e056d181c35e48aa01ad4f8090bdb84f78dca42f47dc",
        # fix runner.bash.template always returning success, format MODULE.bazel and WORKSPACE.bzlmod too
        patches = ["@rules_haskell//buildifier:buildifier_test-workspace.patch"],
        patch_args = ["-p1"],
        strip_prefix = "buildtools-6.1.2",
        urls = ["https://github.com/bazelbuild/buildtools/archive/v6.1.2.tar.gz"],
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
        sha256 = "c3e5e9fdd5004dcb542feda5ee4f0ff0744628baf8ed2dd5d66f8ca1197cb1a1",
        strip_prefix = "zlib-1.2.11",
        urls = [
            "https://mirror.bazel.build/zlib.net/zlib-1.2.11.tar.gz",
            "http://zlib.net/zlib-1.2.11.tar.gz",
        ],
    )

def _non_module_deps_impl(_ctx):
    repositories(bzlmod = True)

non_module_deps = module_extension(
    implementation = _non_module_deps_impl,
)
