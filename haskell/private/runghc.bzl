load("@bazel_skylib//lib:paths.bzl", "paths")

# Note [Running Setup.hs when cross-compiling]
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# When cross compiling we interpret Setup.hs in the execution
# platform. In order to achieve this, we provide a runghc of a
# toolchain targeting the execution platform.
#
# Producing runghc with a custom rule, as defined here, allows to use
# it in rules which are using a Haskell toolchain for the target
# platform.
#

def _runghc_impl(ctx):
    hs_toolchain = ctx.toolchains["@rules_haskell//haskell:toolchain"]
    (_, extension) = paths.split_extension(hs_toolchain.tools.runghc.path)

    runghc_file = ctx.actions.declare_file(ctx.label.name + extension)
    ctx.actions.symlink(
        output = runghc_file,
        target_file = hs_toolchain.tools.runghc,
        is_executable = True,
    )

    return [DefaultInfo(
        executable = runghc_file,
        runfiles = hs_toolchain.cc_wrapper.runfiles.merge(
            ctx.runfiles(files = [runghc_file, hs_toolchain.tools.runghc]),
        ),
    )]

runghc = rule(
    executable = True,
    implementation = _runghc_impl,
    toolchains = ["@rules_haskell//haskell:toolchain"],
    doc = """Produces the runghc program.""",
)
