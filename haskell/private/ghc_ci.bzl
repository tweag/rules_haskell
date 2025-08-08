load("@rules_haskell//haskell:ghc.bzl", "DEFAULT_GHC_VERSION")

def _init_ghc_version_repository(repository_ctx, GHC_VERSION):
    repository_ctx.file("BUILD")
    repository_ctx.file("ghc_version.bzl", content = "GHC_VERSION = {}".format(repr(GHC_VERSION)))

def _ghc_version_impl(repository_ctx):
    GHC_VERSION = repository_ctx.os.environ.get("GHC_VERSION")

    if GHC_VERSION:
        # buildifier: disable=print
        print("Using GHC version {} from env variable `GHC_VERSION`".format(GHC_VERSION))

    _init_ghc_version_repository(repository_ctx, GHC_VERSION)

ghc_version = repository_rule(
    implementation = _ghc_version_impl,
    environ = ["GHC_VERSION"],
    configure = True,
)

def _ghc_default_version_impl(repository_ctx):
    _init_ghc_version_repository(repository_ctx, DEFAULT_GHC_VERSION)

ghc_default_version = repository_rule(
    implementation = _ghc_default_version_impl,
)
