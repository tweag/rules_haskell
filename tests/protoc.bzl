load("@rules_cc//cc:find_cc_toolchain.bzl", "find_cc_toolchain", "use_cc_toolchain")

def _protoc_wrapper_impl(ctx):
    """
    Wraps the default protoc compiler, adding the bin directory of the
    cc toolchain's compiler to the PATH as this directory also contains
    the DLLs needed for running executables.
    """
    cc = find_cc_toolchain(ctx)
    compiler = cc.compiler_executable

    protoc_wrapper_file = ctx.actions.declare_file(ctx.label.name)
    ctx.actions.write(
        output = protoc_wrapper_file,
        content = """\
@echo on

Setlocal EnableDelayedExpansion

For %%G in ("{compiler_exe}") do set protoc_path=%%~dpG
path %path%;%protoc_path%

{protoc} %*
""".format(protoc = repr(ctx.executable._protoc.path), compiler_exe = compiler),
        is_executable = True,
    )
    return [DefaultInfo(
        executable = protoc_wrapper_file,
        runfiles = ctx.runfiles(files = [protoc_wrapper_file, ctx.executable._protoc]),
    )]

_protoc_wrapper = rule(
    executable = True,
    implementation = _protoc_wrapper_impl,
    attrs = {
        "_protoc": attr.label(executable = True, cfg = "exec", default = "@com_google_protobuf//:protoc"),
        "_cc_toolchain": attr.label(
            default = Label(
                "@rules_cc//cc:current_cc_toolchain",
            ),
        ),
    },
    toolchains = use_cc_toolchain(),
)

def protoc_wrapper(name):
    _protoc_wrapper(name = name + "wrapper.cmd")
    native.sh_binary(
        name = name + ".cmd",
        srcs = [name + "wrapper.cmd"],
        target_compatible_with = ["@platforms//os:windows"],
    )
