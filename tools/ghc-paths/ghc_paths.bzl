load("@bazel_skylib//lib:paths.bzl", "paths")

def _ghc_paths_impl(ctx):
    hs = ctx.toolchains["@rules_haskell//haskell:toolchain"]
    ctx.actions.expand_template(
        template = ctx.file.template,
        output = ctx.outputs.output,
        substitutions = {
            "%GHC%": paths.join(ctx.workspace_name, hs.tools.ghc.path),
            "%GHC_PKG%": paths.join(ctx.workspace_name, hs.tools.ghc_pkg.path),
            "%LIBDIR%": paths.join(ctx.workspace_name, hs.libdir_path),
            "%DOCDIR%": paths.join(ctx.workspace_name, hs.docdir_path),
        },
    )
    return [DefaultInfo(
        files = depset(direct = [ctx.outputs.output]),
    )]

ghc_paths = rule(
    _ghc_paths_impl,
    attrs = {
        "template": attr.label(allow_single_file = True),
        "output": attr.output(),
    },
    toolchains = ["@rules_haskell//haskell:toolchain"],
)

def _ghc_files_impl(ctx):
    hs = ctx.toolchains["@rules_haskell//haskell:toolchain"]
    if ctx.attr.component == "bin":
        files = hs.bindir
    elif ctx.attr.component == "doc":
        files = hs.docdir
    elif ctx.attr.component == "lib":
        files = hs.libdir
    else:
        fail("Unknown component", "component")
    return [DefaultInfo(
        files = depset(files),
        runfiles = ctx.runfiles(files = files),
    )]

ghc_files = rule(
    _ghc_files_impl,
    attrs = {
        "component": attr.string(
            doc = "One of `bin`, `doc`, `lib`.",
            mandatory = True,
        ),
    },
    toolchains = ["@rules_haskell//haskell:toolchain"],
)
