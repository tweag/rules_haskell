"""Runs ghc --help"""

hs_toolchain = "@io_tweag_rules_haskell//haskell:toolchain"

def _impl(ctx):
    output = ctx.outputs.out
    ghc = ctx.toolchains[hs_toolchain].tools.ghc
    ctx.actions.run_shell(
        inputs = [ghc],
        outputs = [output],
        progress_message = "Printing ghc help message",
        command = "%s --help > %s" % (ghc.path, output.path),
    )

ghc_help = rule(
    implementation = _impl,
    outputs = {"out": "out_file"},
    toolchains = [hs_toolchain],
)
