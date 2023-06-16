load(
    "@rules_haskell//haskell:ghc_bindist.bzl",
    "LOCAL_PYTHON_REPO_NAME",
    "configure_python3_toolchain",
)
load("@rules_haskell//tools:repositories.bzl", "rules_haskell_worker_dependencies")
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

def repositories(*, bzlmod):
    if LOCAL_PYTHON_REPO_NAME not in native.existing_rules():
        configure_python3_toolchain(name = LOCAL_PYTHON_REPO_NAME, register = not bzlmod)

    # For persistent worker (tools/worker)
    # [TODO] make this customizable via a module extension so that users
    # of persistant workers can use dependencies compatible with the
    # selected toolchain.
    rules_haskell_worker_dependencies()

    # TODO: Remove when tests are run with a ghc version containing Cabal >= 3.10
    # See https://github.com/tweag/rules_haskell/issues/1871
    http_archive(
        name = "Cabal",
        build_file_content = """
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_library")
haskell_cabal_library(
    name = "Cabal",
    srcs = glob(["Cabal/**"]),
    verbose = False,
    version = "3.6.3.0",
    visibility = ["//visibility:public"],
)
""",
        sha256 = "f69b46cb897edab3aa8d5a4bd7b8690b76cd6f0b320521afd01ddd20601d1356",
        strip_prefix = "cabal-gg-8220-with-3630",
        urls = ["https://github.com/tweag/cabal/archive/refs/heads/gg/8220-with-3630.zip"],
    )

def _non_module_deps_impl(_ctx):
    repositories(bzlmod = True)

non_module_deps = module_extension(
    implementation = _non_module_deps_impl,
)
