load(
    "@io_tweag_rules_haskell//haskell:private/providers.bzl",
    "HaskellBinaryInfo",
    "HaskellBuildInfo",
)
load("//haskell:private/set.bzl", "set")

def _get_libraries_as_runfiles_impl(ctx):
    """Extract all library files from a Haskell binary target
    and put them in this target’s files"""
    bi = ctx.attr.binary[HaskellBuildInfo]
    external_libs = [
        lib.mangled_lib
        for lib in set.to_list(bi.external_libraries)
    ]
    return [DefaultInfo(
        # not necessarily complete
        files = depset(
            direct = external_libs + bi.static_libraries,
            transitive = [set.to_depset(bi.dynamic_libraries)],
        ),
    )]

get_libraries_as_runfiles = rule(
    _get_libraries_as_runfiles_impl,
    attrs = {
        "binary": attr.label(
            mandatory = True,
            providers = [HaskellBuildInfo, HaskellBinaryInfo],
        ),
    },
)
