load("@bazel_skylib//:lib.bzl", "paths")
load(":private/context.bzl", "haskell_context")

def _impl(ctx):
  """Create empty archive so that GHC has some input files to work on during
  linking.

  See: https://github.com/facebook/buck/blob/126d576d5c07ce382e447533b57794ae1a358cc2/src/com/facebook/buck/haskell/HaskellDescriptionUtils.java#L295

  Returns:
    File, the created dummy archive.
  """

  # TODO: this is a duplication from ./haskell_impl.bzl
  hs = haskell_context(ctx)
  cc_toolchain = ctx.toolchains["@bazel_tools//tools/cpp:toolchain_type"]

  dummy_raw = "BazelDummy.hs"
  dummy_input = hs.actions.declare_file(dummy_raw)
  dummy_object = hs.actions.declare_file(paths.replace_extension(dummy_raw, ".o"))

  hs.actions.write(output=dummy_input, content="""
{-# LANGUAGE NoImplicitPrelude #-}
module BazelDummy () where
""")

  hs.toolchain.actions.run_ghc(
    hs,
    inputs = [dummy_input],
    outputs = [dummy_object],
    mnemonic = "HaskellDummyObjectGhc",
    arguments = [
      "-optc" + f for f in cc_toolchain.compiler_options()
    ] + ["-c", dummy_input.path],
  )

  ar_args = hs.actions.args()
  ar_args.add(["qc", ctx.outputs.archive, dummy_object])

  hs.actions.run(
    inputs = [dummy_object] + hs.tools_runfiles.ar,
    outputs = [ctx.outputs.archive],
    mnemonic = "HaskellDummyObjectAr",
    executable = hs.tools.ar,
    arguments = [ar_args]
  )


create_dummy_archive = rule(
  implementation = _impl,
  toolchains = [
    "@io_tweag_rules_haskell//haskell:toolchain",
    "@bazel_tools//tools/cpp:toolchain_type",
  ],
  outputs = {
    "archive": "libempty.a"
  }
)
