def _ghc_version_impl(repository_ctx):
    ghc_version = repository_ctx.os.environ.get("GHC_VERSION")

    if ghc_version:
        print("Using GHC version {} from env variable `GHC_VERSION`".format(ghc_version))

    repository_ctx.file("BUILD")
    repository_ctx.file("ghc_version.bzl", content = "GHC_VERSION = {}".format(repr(ghc_version)))

ghc_version = repository_rule(
    implementation = _ghc_version_impl,
    environ = ["GHC_VERSION"],
    configure = True,
)
