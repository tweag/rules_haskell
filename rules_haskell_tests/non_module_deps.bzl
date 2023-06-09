""" External repositories for the CI that need to be shared between WORKSPACE and MODULE.bazel files """

load("@rules_haskell//tools:os_info.bzl", "os_info")
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load(
    "//tests/external-haskell-repository:workspace_dummy.bzl",
    "haskell_package_repository_dummy",
)
load("@rules_haskell//tools:repositories.bzl", "rules_haskell_worker_dependencies")

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

    # For persistent worker (tools/worker)
    rules_haskell_worker_dependencies()

    # module rules_bazel_integration_test requires bazel >= 6.1.0
    http_archive(
        name = "contrib_rules_bazel_integration_test",
        sha256 = "f80c4052df80e9099ed0f2f27ef4084604333566a7b028f524ceae6e5569b429",
        strip_prefix = "rules_bazel_integration_test-7ee995a20bbaa2f6540103c63ff4891166133c2f",
        urls = [
            "https://github.com/bazel-contrib/rules_bazel_integration_test/archive/7ee995a20bbaa2f6540103c63ff4891166133c2f.zip",
        ],
    )

    http_archive(
        name = "cgrindel_bazel_starlib",
        sha256 = "ee0033d029b5eaddc21836b2944cf37c95eb5f214eb39834136a316dbc252a73",
        urls = [
            "https://github.com/cgrindel/bazel-starlib/releases/download/v0.16.0/bazel-starlib.v0.16.0.tar.gz",
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
        sha256 = "614c84128ddb86aab4e1f25ba2e027d32fd5c6da302ae30685b9d7973b13da1b",
        strip_prefix = "buildtools-4.2.3",
        urls = ["https://github.com/bazelbuild/buildtools/archive/4.2.3.tar.gz"],
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

    # Does not support bzlmod yet
    http_archive(
        name = "io_bazel_stardoc",
        sha256 = "3fd8fec4ddec3c670bd810904e2e33170bedfe12f90adf943508184be458c8bb",
        urls = [
            "https://mirror.bazel.build/github.com/bazelbuild/stardoc/releases/download/0.5.3/stardoc-0.5.3.tar.gz",
            "https://github.com/bazelbuild/stardoc/releases/download/0.5.3/stardoc-0.5.3.tar.gz",
        ],
    )

def _non_module_deps_impl(_ctx):
    repositories(bzlmod = True)

non_module_deps = module_extension(
    implementation = _non_module_deps_impl,
)
