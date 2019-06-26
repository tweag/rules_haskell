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

ghc, ghc_pkg, docdir, libdir :: FilePath

ghc     = "{ghc}"
ghc_pkg = "{ghc_pkg}"

docdir  = "DOCDIR_IS_NOT_SET"
EOM

      echo -n 'libdir  = "' >> {out}
      {ghc} --print-libdir | tr '\\' '/' | tr -d '[:space:]' >> {out}
      echo '"' >> {out}
""".format(
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
