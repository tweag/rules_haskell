load("@bazel_skylib//lib:paths.bzl", "paths")

def _ghc_paths_impl(ctx):
    hs = ctx.toolchains["@rules_haskell//haskell:toolchain"]
    print("ws:", ctx.workspace_name)
    print("ws:", ctx.label.workspace_root)
    print("ghc:", hs.tools.ghc.path)
    print("ghc_pkg:", hs.tools.ghc_pkg.path)
    print("libdir:", hs.libdir_path)
    print("libdir2:", paths.join(hs.tools.ghc.root.path, hs.libdir_path))
    ctx.actions.expand_template(
        template = ctx.file.template,
        output = ctx.outputs.output,
        substitutions = {
            "%GHC%": paths.join(ctx.workspace_name, hs.tools.ghc.short_path),
            "%GHC_PKG%": paths.join(ctx.workspace_name, hs.tools.ghc_pkg.short_path),
            "%LIBDIR%": paths.join(ctx.workspace_name, paths.dirname(paths.dirname(hs.tools.ghc.short_path)), hs.libdir_path),
            "%DOCDIR%": paths.join(ctx.workspace_name, paths.dirname(paths.dirname(hs.tools.ghc.short_path)), hs.docdir_path),
        },
    )
    return [DefaultInfo(
        files = depset(direct = [ctx.outputs.output]),
        #runfiles = ctx.runfiles(collect_default = True),
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
