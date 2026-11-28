"""Exposes the Cabal outputs that must not depend on the execroot location."""

load("@rules_haskell//haskell:providers.bzl", "HaskellInfo")

def _cabal_outputs_impl(ctx):
    """Collect the package database and static library of each library.

    Files of the libraries' dependencies are left out.
    """
    files = []
    for lib in ctx.attr.libraries:
        info = lib[HaskellInfo]
        files.extend([
            f
            for f in info.package_databases.to_list() + info.hs_libraries.to_list()
            if f.owner == lib.label and (f.basename == "package.cache" or f.extension == "a")
        ])
    return [DefaultInfo(files = depset(files))]

cabal_outputs = rule(
    _cabal_outputs_impl,
    attrs = {
        "libraries": attr.label_list(
            mandatory = True,
            providers = [HaskellInfo],
        ),
    },
)
