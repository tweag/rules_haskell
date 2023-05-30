load(
    "@rules_haskell//haskell:providers.bzl",
    "HaskellInfo",
    "HaskellLibraryInfo",
)
load("//haskell:private/set.bzl", "set")

def _get_libraries_as_runfiles_impl(ctx):
    """Extract all library files from a haskell_library target
    and put them in this target’s files"""
    bi = ctx.attr.library[HaskellInfo]
    return [DefaultInfo(
        # not necessarily complete
        files = depset(
            transitive = [bi.hs_libraries],
        ),
    )]

get_libraries_as_runfiles = rule(
    _get_libraries_as_runfiles_impl,
    attrs = {
        "library": attr.label(
            mandatory = True,
            providers = [HaskellInfo, HaskellLibraryInfo],
        ),
    },
)
