load("@bazel_skylib//lib:paths.bzl", "paths")
load("@rules_python//python:defs.bzl", "py_binary")

# Note [Running Setup.hs when cross-compiling]
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# When cross compiling we interpret Setup.hs in the execution
# platform. In order to achieve this, we provide a runghc of a
# toolchain targeting the execution platform.
#
# Producing a runghc wrapper with a custom rule, as defined here,
# allows to use it in rules which are using a Haskell toolchain for
# the target platform.
#

def _runghc_wrapper_impl(ctx):
    hs_toolchain = ctx.toolchains["@rules_haskell//haskell:toolchain"]

    f = hs_toolchain.tools.runghc
    runghc_runfile_path = paths.join(f.owner.workspace_name, f.owner.package, hs_toolchain.tools_path, "runghc")
    runghc_wrapper_file = ctx.actions.declare_file(ctx.label.name)
    ctx.actions.write(
        output = runghc_wrapper_file,
        content = """\
#!/usr/bin/env python3

import subprocess
import sys
from rules_python.python.runfiles import runfiles

r = runfiles.Create()

subprocess.run([r.Rlocation("{runghc}")] + sys.argv[1:], check=True)
""".format(runghc = runghc_runfile_path),
        is_executable = True,
    )

    return [DefaultInfo(
        executable = runghc_wrapper_file,
        runfiles = hs_toolchain.cc_wrapper.runfiles.merge(
            ctx.runfiles(files = [runghc_wrapper_file, hs_toolchain.tools.runghc]),
        ),
    )]

_runghc_wrapper = rule(
    executable = True,
    implementation = _runghc_wrapper_impl,
    toolchains = ["@rules_haskell//haskell:toolchain"],
    doc = """Produces the runghc wrapper script.""",
)

def runghc(name, **kwargs):
    _runghc_wrapper(name = name + ".py")
    py_binary(
        name = name,
        srcs = [name + ".py"],
        data = [name + ".py"],
        srcs_version = "PY3",
        python_version = "PY3",
        deps = [
            "@rules_python//python/runfiles",
        ],
        **kwargs
    )
