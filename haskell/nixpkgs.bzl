"""Workspace rules (Nixpkgs)"""

load(
    "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_package",
    "nixpkgs_sh_posix_configure",
)

def _ghc_nixpkgs_haskell_toolchain_impl(repository_ctx):
    compiler_flags_select = "select({})".format(
        repository_ctx.attr.compiler_flags_select or {
            "//conditions:default": [],
        },
    )
    locale_archive = repository_ctx.attr.locale_archive
    nixpkgs_ghc_path = repository_ctx.path(repository_ctx.attr._nixpkgs_ghc).dirname.dirname

    # Symlink content of ghc external repo. In effect, this repo has
    # the same content, but with a BUILD file that includes generated
    # content (not a static one like nixpkgs_package supports).
    for target in _find_children(repository_ctx, nixpkgs_ghc_path):
        basename = target.rpartition("/")[-1]
        repository_ctx.symlink(target, basename)

    # Generate BUILD file entries describing each prebuilt package.
    pkgdb_to_bzl = repository_ctx.path(Label("@rules_haskell//haskell:private/pkgdb_to_bzl.py"))
    ghc_name = "ghc-{}".format(repository_ctx.attr.version)
    result = repository_ctx.execute(["ls", "lib"])
    if result.return_code or not ghc_name in result.stdout.splitlines():
        fail(
            """\
GHC version does not match expected version.
You specified {wanted}.
Available versions:
{actual}
""".format(wanted = ghc_name, actual = result.stdout),
        )
    result = repository_ctx.execute([
        pkgdb_to_bzl,
        repository_ctx.attr.name,
        "lib/{}".format(ghc_name),
    ])
    if result.return_code:
        fail("Error executing pkgdb_to_bzl.py: {stderr}".format(stderr = result.stderr))
    toolchain_libraries = result.stdout

    # Haddock files on nixpkgs are stored outside of the ghc package
    # The pkgdb_to_bzl.py program generates bazel labels for theses files
    # and asks the parent process to generate the associated bazel symlink
    for line in result.stdout.split("\n"):
        if line.startswith("#SYMLINK:"):
            _, path, name = line.split(" ")
            repository_ctx.symlink(path, name)

    repository_ctx.file(
        "BUILD",
        executable = False,
        content = """
load(
    "@rules_haskell//haskell:defs.bzl",
    "haskell_import",
    "haskell_toolchain",
)

package(default_visibility = ["//visibility:public"])

filegroup(
    name = "bin",
    srcs = glob(["bin/*"]),
)

{toolchain_libraries}

haskell_toolchain(
    name = "toolchain-impl",
    tools = {tools},
    libraries = toolchain_libraries,
    version = "{version}",
    is_static = {is_static},
    compiler_flags = {compiler_flags} + {compiler_flags_select},
    haddock_flags = {haddock_flags},
    repl_ghci_args = {repl_ghci_args},
    # On Darwin we don't need a locale archive. It's a Linux-specific
    # hack in Nixpkgs.
    {locale_archive_arg}
    locale = {locale},
)
        """.format(
            toolchain_libraries = toolchain_libraries,
            tools = ["@rules_haskell_ghc_nixpkgs//:bin"],
            version = repository_ctx.attr.version,
            is_static = repository_ctx.attr.is_static,
            compiler_flags = repository_ctx.attr.compiler_flags,
            compiler_flags_select = compiler_flags_select,
            haddock_flags = repository_ctx.attr.haddock_flags,
            repl_ghci_args = repository_ctx.attr.repl_ghci_args,
            locale_archive_arg = "locale_archive = {},".format(repr(locale_archive)) if locale_archive else "",
            locale = repr(repository_ctx.attr.locale),
        ),
    )

_ghc_nixpkgs_haskell_toolchain = repository_rule(
    _ghc_nixpkgs_haskell_toolchain_impl,
    attrs = {
        # These attributes just forward to haskell_toolchain.
        # They are documented there.
        "version": attr.string(),
        "is_static": attr.bool(),
        "compiler_flags": attr.string_list(),
        "compiler_flags_select": attr.string_list_dict(),
        "haddock_flags": attr.string_list(),
        "repl_ghci_args": attr.string_list(),
        "locale_archive": attr.string(),
        # Unfortunately, repositories cannot depend on each other
        # directly. They can only depend on files inside each
        # repository. We need to be careful to depend on files that
        # change anytime any content in a repository changes, like
        # bin/ghc, which embeds the output path, which itself changes
        # if any input to the derivation changed.
        "_nixpkgs_ghc": attr.label(default = "@rules_haskell_ghc_nixpkgs//:bin/ghc"),
        "locale": attr.string(
            default = "C.UTF-8",
        ),
    },
)

def _ghc_nixpkgs_toolchain_impl(repository_ctx):
    # These constraints might look tautological, because they always
    # match the host platform if it is the same as the target
    # platform. But they are important to state because Bazel
    # toolchain resolution prefers other toolchains with more specific
    # constraints otherwise.
    target_constraints = ["@platforms//cpu:x86_64"]
    if repository_ctx.os.name == "linux":
        target_constraints.append("@platforms//os:linux")
    elif repository_ctx.os.name == "mac os x":
        target_constraints.append("@platforms//os:osx")
    exec_constraints = list(target_constraints)
    exec_constraints.append("@io_tweag_rules_nixpkgs//nixpkgs/constraints:support_nix")

    repository_ctx.file(
        "BUILD",
        executable = False,
        content = """
toolchain(
    name = "toolchain",
    toolchain_type = "@rules_haskell//haskell:toolchain",
    toolchain = "@rules_haskell_ghc_nixpkgs_haskell_toolchain//:toolchain-impl",
    exec_compatible_with = {exec_constraints},
    target_compatible_with = {target_constraints},
)
        """.format(
            exec_constraints = exec_constraints,
            target_constraints = target_constraints,
        ),
    )

_ghc_nixpkgs_toolchain = repository_rule(_ghc_nixpkgs_toolchain_impl)

def haskell_register_ghc_nixpkgs(
        version,
        is_static = False,
        build_file = None,
        build_file_content = None,
        compiler_flags = None,
        compiler_flags_select = None,
        haddock_flags = None,
        repl_ghci_args = None,
        locale_archive = None,
        attribute_path = "haskellPackages.ghc",
        sh_posix_attributes = None,
        nix_file = None,
        nix_file_deps = [],
        nixopts = None,
        locale = None,
        repositories = {},
        repository = None,
        nix_file_content = None):
    """Register a package from Nixpkgs as a toolchain.

    Toolchains can be used to compile Haskell code. To have this
    toolchain selected during [toolchain
    resolution][toolchain-resolution], set a host platform that
    includes the
    `@io_tweag_rules_nixpkgs//nixpkgs/constraints:support_nix`
    constraint value.

    [toolchain-resolution]: https://docs.bazel.build/versions/master/toolchains.html#toolchain-resolution

    ### Examples

      ```
      haskell_register_ghc_nixpkgs(
          locale_archive = "@glibc_locales//:locale-archive",
          atttribute_path = "haskellPackages.ghc",
          version = "1.2.3",   # The version of GHC
      )
      ```

      Setting the host platform can be done on the command-line like
      in the following:

      ```
      --host_platform=@io_tweag_rules_nixpkgs//nixpkgs/platforms:host
      ```

    Args:
      compiler_flags_select: temporary workaround to pass conditional arguments.
        See https://github.com/bazelbuild/bazel/issues/9199 for details.
      sh_posix_attributes: List of attribute paths to extract standard Unix shell tools from.
        Passed to nixpkgs_sh_posix_configure.
    """
    nixpkgs_ghc_repo_name = "rules_haskell_ghc_nixpkgs"
    nixpkgs_sh_posix_repo_name = "rules_haskell_sh_posix_nixpkgs"
    haskell_toolchain_repo_name = "rules_haskell_ghc_nixpkgs_haskell_toolchain"
    toolchain_repo_name = "rules_haskell_ghc_nixpkgs_toolchain"

    # The package from the system.
    nixpkgs_package(
        name = nixpkgs_ghc_repo_name,
        attribute_path = attribute_path,
        build_file = build_file,
        build_file_content = build_file_content,
        nix_file = nix_file,
        nix_file_deps = nix_file_deps,
        nix_file_content = nix_file_content,
        nixopts = nixopts,
        repositories = repositories,
        repository = repository,
    )

    # haskell_toolchain + haskell_import definitions.
    _ghc_nixpkgs_haskell_toolchain(
        name = haskell_toolchain_repo_name,
        version = version,
        is_static = is_static,
        compiler_flags = compiler_flags,
        compiler_flags_select = compiler_flags_select,
        haddock_flags = haddock_flags,
        repl_ghci_args = repl_ghci_args,
        locale_archive = locale_archive,
        locale = locale,
    )

    # toolchain definition.
    _ghc_nixpkgs_toolchain(name = toolchain_repo_name)
    native.register_toolchains("@{}//:toolchain".format(toolchain_repo_name))

    # Unix tools toolchain required for Cabal packages
    sh_posix_nixpkgs_kwargs = dict(
        nix_file_deps = nix_file_deps,
        nixopts = nixopts,
        repositories = repositories,
        repository = repository,
    )
    if sh_posix_attributes != None:
        sh_posix_nixpkgs_kwargs["packages"] = sh_posix_attributes
    nixpkgs_sh_posix_configure(
        name = nixpkgs_sh_posix_repo_name,
        **sh_posix_nixpkgs_kwargs
    )

def _find_children(repository_ctx, target_dir):
    find_args = [
        "find",
        "-L",
        target_dir,
        "-maxdepth",
        "1",
        # otherwise the directory is printed as well
        "-mindepth",
        "1",
        # filenames can contain \n
        "-print0",
    ]
    exec_result = repository_ctx.execute(find_args)
    if exec_result.return_code:
        fail("_find_children() failed.")
    return exec_result.stdout.rstrip("\0").split("\0")
