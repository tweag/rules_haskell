def _ghc_paths_module_impl(ctx):
    tools = ctx.toolchains["@io_tweag_rules_haskell//haskell:toolchain"].tools
    ghc = tools.ghc
    ctx.actions.run_shell(
        inputs = [ghc],
        outputs = [ctx.outputs.out],
        command = """
      cat > {out} << EOM
module GHC.Paths (
        ghc, ghc_pkg, libdir, docdir
  ) where

libdir, docdir, ghc, ghc_pkg :: FilePath

libdir  = "$({ghc} --print-libdir | sed s:\\:/:g)"
docdir  = "DOCDIR_IS_NOT_SET"

ghc     = "{ghc}"
ghc_pkg = "{ghc_pkg}"
EOM""".format(
            ghc = ghc.path,
            ghc_pkg = tools.ghc_pkg.path,
            out = ctx.outputs.out.path,
        ),
    )
    return [DefaultInfo(runfiles = ctx.runfiles(collect_data = True))]

ghc_paths_module = rule(
    _ghc_paths_module_impl,
    toolchains = ["@io_tweag_rules_haskell//haskell:toolchain"],
    outputs = {"out": "GHC/Paths.hs"},
)
