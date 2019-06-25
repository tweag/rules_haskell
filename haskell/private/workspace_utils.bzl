load("@bazel_tools//tools/cpp:lib_cc_configure.bzl", "get_cpu_value")

def execute_or_fail_loudly(repository_ctx, arguments):
    """Execute the given command

    Fails if the command does not exit with exit-code 0.

    Args:
      arguments: List, the command line to execute.

    Returns:
      exec_result: The output of the command.

    """
    exec_result = repository_ctx.execute(arguments)
    if exec_result.return_code != 0:
        arguments = [_as_string(x) for x in arguments]
        fail("\n".join(["Command failed: " + " ".join(arguments), exec_result.stderr]))
    return exec_result

def _as_string(v):
    if type(v) == "string":
        return v
    else:
        return repr(v)

def _find_ghc(repository_ctx):
    """Find the GHC executable in the current workspace.

    Returns:
      path, The path to the GHC executable.

    """
    if get_cpu_value(repository_ctx) == "x64_windows":
        ghc = repository_ctx.path("bin/ghc.exe")
    else:
        ghc = repository_ctx.path("bin/ghc")

    if not ghc.exists:
        fail("Cannot find GHC executable in {}.".format(ghc))

    return ghc

def ghc_is_static(repository_ctx):
    """Query GHC for whether the RTS is static or dynamic

    Requires the GHC executable to exist under `bin/` in the current workspace.

    Returns:
      Bool, True for static RTS, False for dynamic RTS.

    """
    ghc = _find_ghc(repository_ctx)
    repository_ctx.file(
        "_ghc_is_dynamic.ghci",
        content = 'foreign import ccall unsafe "rts_isDynamic" isDynamic :: IO Int',
        executable = False,
    )
    result = execute_or_fail_loudly(repository_ctx, [
        ghc,
        "--interactive",
        "-ghci-script",
        "_ghc_is_dynamic.ghci",
        "-e",
        "isDynamic",
    ])
    return result.stdout.strip() == "0"
