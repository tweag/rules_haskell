load("//haskell:private/context.bzl", "GHC_BINARIES")
load("//haskell:private/pkgdb_to_bzl.bzl", "pkgdb_to_bzl")
load(
    "//haskell:private/workspace_utils.bzl",
    "define_rule",
    "execute_or_fail_loudly",
    "resolve_labels",
)

# TMP
load("@bazel_tools//tools/cpp:lib_cc_configure.bzl", "get_cpu_value")
load("@rules_sh//sh:posix.bzl", "sh_posix_configure")
load(
    "//haskell:private/workspace_utils.bzl",
    "find_python",
)

def _haskell_configure_impl(repository_ctx):
    paths = resolve_labels(repository_ctx, [
        "@rules_haskell//haskell:ghc.BUILD.tpl",
        "@rules_haskell//haskell:private/pkgdb_to_bzl.py",
    ])

    result = execute_or_fail_loudly(repository_ctx, ["ghc", "--numeric-version"])
    version = result.stdout.strip()
    if repository_ctx.attr.version and version != repository_ctx.attr.version:
        fail(
            """\
GHC version does not match expected version.
You specified {wanted}.
I got {actual}.
""".format(wanted = repository_ctx.attr.version, actual = version),
        )
    result = execute_or_fail_loudly(repository_ctx, ["ghc", "--print-libdir"])
    libdir = repository_ctx.path(result.stdout.strip())
    execute_or_fail_loudly(repository_ctx, ["mkdir", "lib"])
    for dentry in libdir.readdir():
        repository_ctx.symlink(dentry, "lib/{}".format(dentry.basename))
    haskell_toolchain_libraries = pkgdb_to_bzl(repository_ctx, paths, "lib")
    execute_or_fail_loudly(repository_ctx, ["mkdir", "bin"])
    for exe in GHC_BINARIES:
        path = repository_ctx.which(exe)
        if path:
            repository_ctx.symlink(path, "bin/{}".format(exe))

    haskell_toolchain = define_rule(
        "haskell_toolchain",
        name = "toolchain-impl",
        tools = [":bin"],
        libraries = "toolchain_libraries",
        version = repr(version),
        static_runtime = repository_ctx.attr.static_runtime,
        fully_static_link = False,  # XXX not yet supported for host toolchain.
        compiler_flags = repository_ctx.attr.compiler_flags,
        haddock_flags = repository_ctx.attr.haddock_flags,
        repl_ghci_args = repository_ctx.attr.repl_ghci_args,
        cabalopts = repository_ctx.attr.cabalopts,
    )
    repository_ctx.template(
        "BUILD",
        paths["@rules_haskell//haskell:ghc.BUILD.tpl"],
        substitutions = {
            "%{haskell_toolchain_libraries}": haskell_toolchain_libraries,
            "%{haskell_toolchain}": haskell_toolchain,
        },
        executable = False,
    )

_haskell_configure = repository_rule(
    _haskell_configure_impl,
    attrs = {
        "version": attr.string(),
        "static_runtime": attr.bool(),
        "compiler_flags": attr.string_list(),
        "haddock_flags": attr.string_list(),
        "cabalopts": attr.string_list(),
        "repl_ghci_args": attr.string_list(),
        "locale": attr.string(default = "en_US.UTF-8"),
        "locale_archive": attr.string(),
        # Ideally C.UTF-8 would be the default, but it's not supported
        # on macOS.
    },
)

def _host_toolchain_impl(repository_ctx):
    toolchain = define_rule(
        "toolchain",
        name = "toolchain",
        toolchain_type = repr("@rules_haskell//haskell:toolchain"),
        toolchain = repr(str(repository_ctx.attr.haskell_toolchain)),
        exec_compatible_with = "HOST_CONSTRAINTS",
        target_compatible_with = "HOST_CONSTRAINTS",
    )
    print(toolchain)
    repository_ctx.file(
        "BUILD",
        executable = False,
        content = """
load("@local_config_platform//:constraints.bzl", "HOST_CONSTRAINTS")

{toolchain}
        """.format(toolchain = toolchain),
    )

_host_toolchain = repository_rule(
    _host_toolchain_impl,
    attrs = {
        "haskell_toolchain": attr.label(),
    },
)

def haskell_configure(
        version = None,
        static_runtime = None,
        compiler_flags = None,
        haddock_flags = None,
        cabalopts = None,
        repl_ghci_args = None,
        locale = None,
        locale_archive = None):
    """Detect any Haskell toolchain available in the host environment.

    The rule writes out configuration data based on what was
    autodetected. This rule is mutually exclusive with bindists.

    ### Examples

      ```
      haskell_configure(
          version = "1.2.3",  # Optional. Set only to enforce a GHC version.

      )
      ```

    """
    _haskell_configure(
        name = "rules_haskell_ghc_host",
        version = version,
        static_runtime = static_runtime,
        compiler_flags = compiler_flags,
        haddock_flags = haddock_flags,
        cabalopts = cabalopts,
        repl_ghci_args = repl_ghci_args,
        locale = locale,
        locale_archive = locale_archive,
    )
    _host_toolchain(
        name = "rules_haskell_ghc_host_toolchain",
        haskell_toolchain = "@rules_haskell_ghc_host//:toolchain-impl",
    )
    native.register_toolchains("@rules_haskell_ghc_host_toolchain//:toolchain")
