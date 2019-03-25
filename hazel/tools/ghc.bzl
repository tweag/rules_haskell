load("@bazel_tools//tools/cpp:lib_cc_configure.bzl", "get_cpu_value")

default_ghc_workspaces = {
    "k8": "@ghc",
    "darwin": "@ghc",
    "x64_windows": "@io_tweag_rules_haskell_ghc_windows_amd64",

}

def get_ghc_workspace(ghc_workspaces, repository_ctx):
    """Return the GHC workspace appropriate for the current OS."""
    cpu_value = get_cpu_value(repository_ctx)
    if cpu_value not in ghc_workspaces:
        fail("No known GHC workspace for CPU {} in {}".format(
            cpu_value,
            ghc_workspaces,
        ))
    return ghc_workspaces[cpu_value]

def get_executable_name(name, repository_ctx):
    """Return the executable name for the current platform.

    On Windows, appends `.exe` to `name`. Otherwise, returns `name`.
    """
    if get_cpu_value(repository_ctx) == "x64_windows":
        return "%s.exe" % name
    else:
        return name
