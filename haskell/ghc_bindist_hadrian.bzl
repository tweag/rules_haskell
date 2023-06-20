"""Workspace rules (GHC binary distributions)"""

load("@bazel_tools//tools/cpp:lib_cc_configure.bzl", "get_cpu_value")
load("@rules_sh//sh:posix.bzl", "sh_posix_configure")
load(
    ":private/pkgdb_to_bzl.bzl",
    "pkgdb_to_bzl",
)
load(":private/versions.bzl", "check_bazel_version")
load(
    ":private/workspace_utils.bzl",
    "define_rule",
    "find_python",
    "resolve_labels",
)

# A rule to copy a filegroup to the current execroot.
# This rule is generic and should probably be moved in its own file.
# Borrowed from https://github.com/lewish/asciiflow/blob/master/copy_files.bzl

def _copy_filegroup_impl(ctx):
    all_input_files = depset(ctx.files.srcs).to_list()

    all_outputs = []
    for f in all_input_files:
        output_path = f.short_path
        out = ctx.actions.declare_file(output_path)
        all_outputs.append(out)
        ctx.actions.run_shell(
            outputs = [out],
            inputs = depset([f]),
            arguments = [f.path, out.path],
            command = "mkdir -p $(dirname $2) && cp $1 $2",
        )

    return [
        DefaultInfo(
            files = depset(all_outputs),
            runfiles = ctx.runfiles(files = all_outputs),
        ),
    ]

copy_filegroups_to_this_package = rule(
    implementation = _copy_filegroup_impl,
    attrs = {
        "srcs": attr.label_list(),
    },
)

# Targets are of the form x86_64-unknown-linux.
def _split_target(target):
    arch, _, os = target.split("-")
    return (arch, os)

# Main function.
# It fills the template ghc.BUILD.tpl with the targets specific to the platform the build is done on.
def _ghc_bindist_hadrian_impl(ctx):
    filepaths = resolve_labels(ctx, [
        "@rules_haskell//haskell:ghc.BUILD.tpl",
        "@rules_haskell//haskell:private/pkgdb_to_bzl.py",
    ])
    _, os = _split_target(ctx.attr.target)
    unpack_dir = ""

    ctx.download_and_extract(
        url = ctx.attr.url,
        output = unpack_dir,
        sha256 = ctx.attr.sha256,
        type = "tar.xz",
        stripPrefix = ctx.attr.strip_prefix,
    )

    bindir = "bin"
    libdir = "lib"
    docdir = None
    for d in ["doc", "docs", "docs/html"]:
        if ctx.path(d).exists:
            if ctx.path(d).get_child("index.html").exists:
                docdir = d
                break

    # The bindist requires patching invalid haddock paths. See Makefile of GHC:
    # https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.2.3-release/hadrian/bindist/Makefile#L54-74
    result = ctx.execute(["bash", "-c", """\
set -euo pipefail
find {lib}/package.conf.d -name "*.conf" -print0 | \\
  xargs -0 sed -i.bak 's|\\${{pkgroot}}/\\.\\./\\.\\.|${{pkgroot}}/..|'
find {lib}/package.conf.d -name "rts-*.conf" -print0 | \\
  xargs -0 sed -i.bak2 's|haddock-html:.*$|haddock-html:|'
""".format(
        lib = libdir,
    )])
    if result.return_code != 0:
        fail(result.stderr)

    toolchain_libraries = pkgdb_to_bzl(ctx, filepaths, libdir)["file_content"]
    locale = ctx.attr.locale or ("en_US.UTF-8" if os == "darwin" else "C.UTF-8")
    toolchain = define_rule(
        "haskell_toolchain",
        name = "toolchain-impl",
        tools = [":generated_bin_filegroup"],
        tools_path = repr(bindir),
        libraries = "toolchain_libraries",
        # See Note [GHC toolchain files]
        libdir = [":generated_lib_filegroup"],
        docdir = [":generated_docdir_filegroup"],
        includedir = [":generated_include_filegroup"],
        libdir_path = repr(libdir),
        docdir_path = repr(docdir),
        version = repr(ctx.attr.version),
        static_runtime = os == "windows",
        fully_static_link = False,  # XXX not yet supported for bindists.
        ghcopts = ctx.attr.ghcopts,
        haddock_flags = ctx.attr.haddock_flags,
        repl_ghci_args = ctx.attr.repl_ghci_args,
        cabalopts = ctx.attr.cabalopts,
        locale = repr(locale),
        hadrian_bindist = True,
    )
    ctx.template(
        "BUILD",
        filepaths["@rules_haskell//haskell:ghc.BUILD.tpl"],
        substitutions = {
            "%{toolchain_libraries}": toolchain_libraries,
            "%{toolchain}": toolchain,
            "%{docdir}": docdir,
            "%{is_clang}": str(False),
        },
        executable = False,
    )

_ghc_bindist_hadrian = repository_rule(
    _ghc_bindist_hadrian_impl,
    local = False,
    attrs = {
        "url": attr.string(),
        "sha256": attr.string(),
        "strip_prefix": attr.string(),
        "version": attr.string(
            doc = "The binary distribution's GHC version",
        ),
        "target": attr.string(mandatory = True),
        "ghcopts": attr.string_list(),
        "haddock_flags": attr.string_list(),
        "repl_ghci_args": attr.string_list(),
        "cabalopts": attr.string_list(),
        "patches": attr.label_list(
            default = [],
            doc =
                "A list of files that are to be applied as patches afer " +
                "extracting the archive.",
        ),
        "patch_tool": attr.string(
            default = "patch",
            doc = "The patch(1) utility to use.",
        ),
        "patch_args": attr.string_list(
            default = ["-p0"],
            doc = "The arguments given to the patch tool",
        ),
        "patch_cmds": attr.string_list(
            default = [],
            doc = "Sequence of commands to be applied after patches are applied.",
        ),
        "locale": attr.string(
            mandatory = False,
        ),
        "_relpath_script": attr.label(
            allow_single_file = True,
            default = Label("@rules_haskell//haskell:assets/relpath.sh"),
        ),
    },
)

def _ghc_bindist_hadrian_toolchain_impl(ctx):
    arch, os = _split_target(ctx.attr.target)
    os_constraint = {
        "darwin": "osx",
        "linux": "linux",
        "windows": "windows",
    }.get(os)
    cpu_constraint = {
        "amd64": "x86_64",
        "arm64": "arm64",
        "x86_64": "x86_64",
    }.get(arch)
    exec_constraints = [
        "@platforms//os:{}".format(os_constraint),
        "@platforms//cpu:{}".format(cpu_constraint),
    ]
    target_constraints = exec_constraints
    ctx.file(
        "BUILD",
        executable = False,
        content = """
toolchain(
    name = "toolchain",
    toolchain_type = "@rules_haskell//haskell:toolchain",
    toolchain = "@{bindist_name}//:toolchain-impl",
    exec_compatible_with = {exec_constraints},
    target_compatible_with = {target_constraints},
)
        """.format(
            bindist_name = ctx.attr.bindist_name,
            exec_constraints = exec_constraints,
            target_constraints = target_constraints,
        ),
    )

_ghc_bindist_hadrian_toolchain = repository_rule(
    _ghc_bindist_hadrian_toolchain_impl,
    local = False,
    attrs = {
        "bindist_name": attr.string(),
        "target": attr.string(),
    },
)

def ghc_bindist_hadrian(
        name,
        url,
        sha256,
        strip_prefix,
        version,
        target,
        ghcopts = None,
        haddock_flags = None,
        repl_ghci_args = None,
        cabalopts = None,
        locale = None):
    bindist_name = name
    toolchain_name = "{}-toolchain".format(name)

    # We want the toolchain definition to be tucked away in a separate
    # repository, that way `bazel build //...` will not match it (and
    # e.g. build the Windows toolchain even on Linux). At the same
    # time, we don't want the definition in the bindist repository,
    # because then we need to download the bindist first before we can
    # see the toolchain definition. The solution is to add the
    # toolchain definition in its own special repository.
    _ghc_bindist_hadrian(
        name = bindist_name,
        url = url,
        sha256 = sha256,
        strip_prefix = strip_prefix,
        version = version,
        ghcopts = ghcopts,
        haddock_flags = haddock_flags,
        repl_ghci_args = repl_ghci_args,
        cabalopts = cabalopts,
        target = target,
        locale = locale,
    )
    _ghc_bindist_hadrian_toolchain(
        name = toolchain_name,
        bindist_name = bindist_name,
        target = target,
    )
    native.register_toolchains("@{}//:toolchain".format(toolchain_name))

def haskell_register_ghc_bindists_hadrian(
        url,
        sha256,
        strip_prefix,
        version,
        target,
        ghcopts = None,
        haddock_flags = None,
        repl_ghci_args = None,
        cabalopts = None,
        locale = None):
    ghc_bindist_hadrian(
        name = "rules_haskell_ghc_{}".format(target),
        url = url,
        sha256 = sha256,
        strip_prefix = strip_prefix,
        target = target,
        version = version,
        ghcopts = ghcopts,
        haddock_flags = haddock_flags,
        repl_ghci_args = repl_ghci_args,
        cabalopts = cabalopts,
        locale = locale,
    )
    local_sh_posix_repo_name = "rules_haskell_sh_posix_local"
    if local_sh_posix_repo_name not in native.existing_rules():
        sh_posix_configure(name = local_sh_posix_repo_name)
    local_python_repo_name = "rules_haskell_python_local"
    if local_python_repo_name not in native.existing_rules():
        _configure_python3_toolchain(name = local_python_repo_name)

def _configure_python3_toolchain_impl(repository_ctx):
    cpu = get_cpu_value(repository_ctx)
    python3_path = find_python(repository_ctx)
    if check_bazel_version("4.2.0")[0]:
        stub_shebang = """stub_shebang = "#!{python3_path}",""".format(
            python3_path = python3_path,
        )
    else:
        stub_shebang = ""
    repository_ctx.file("BUILD.bazel", executable = False, content = """
load(
    "@bazel_tools//tools/python:toolchain.bzl",
    "py_runtime_pair",
)
py_runtime(
    name = "python3_runtime",
    interpreter_path = "{python3}",
    python_version = "PY3",
    {stub_shebang}
)
py_runtime_pair(
    name = "py_runtime_pair",
    py3_runtime = ":python3_runtime",
)
toolchain(
    name = "toolchain",
    toolchain = ":py_runtime_pair",
    toolchain_type = "@bazel_tools//tools/python:toolchain_type",
    exec_compatible_with = [
        "@platforms//cpu:x86_64",
        "@platforms//os:{os}",
    ],
    target_compatible_with = [
        "@platforms//cpu:x86_64",
        "@platforms//os:{os}",
    ],
)
""".format(
        python3 = python3_path,
        os = {
            "darwin": "osx",
            "x64_windows": "windows",
        }.get(cpu, "linux"),
        stub_shebang = stub_shebang,
    ))

_config_python3_toolchain = repository_rule(
    _configure_python3_toolchain_impl,
    configure = True,
    environ = ["PATH"],
)

def _configure_python3_toolchain(name):
    """Autoconfigure python3 toolchain for GHC bindist
    `rules_haskell` requires Python 3 to build Haskell targets. Under Nix we
    use `rules_nixpkgs`'s `nixpkgs_python_configure` repository rule to use a
    nixpkgs provisioned Python toolchain. However, outside of Nix we have to
    rely on whatever Python toolchain is installed on the system.
    Bazel provides `@bazel_tools//tools/python:autodetecting_toolchain` for
    this purpose. However, in its current form, that toolchain does not
    support Python toolchains installed outside of standard system paths
    such as `/usr/bin:/bin:/usr/sbin`. The reason is that the toolchain does
    not look for a Python interpreter in a repository rule. Instead it uses
    wrapper scripts that look for a Python interpreter in `$PATH` within the
    sandboxed build actions.
    On MacOS, which, at the time of writing, only includes Python 2.7, users
    will want to install Python 3 in a non-system path, e.g. via homebrew or
    py_env. The auto detecting toolchain will not find this interpreter and
    builds will fail with the following error:
    ```
    Error occurred while attempting to use the default Python toolchain (@rules_python//python:autodetecting_toolchain).
    According to '/usr/bin/python -V', version is 'Python 2.7.10', but we need version 3. PATH is:
    /usr/bin:/bin:/usr/sbin
    Please ensure an interpreter with version 3 is available on this platform as 'python3' or 'python', or else register an appropriate Python toolchain as per the documentation for py_runtime_pair (https://github.com/bazelbuild/rules_python/blob/master/docs/python.md#py_runtime_pair).
    Note that prior to Bazel 0.27, there was no check to ensure that the interpreter's version matched the version declared by the target (#4815). If your build worked prior to Bazel 0.27, and you're sure your targets do not require Python 3, you can opt out of this version check by using the non-strict autodetecting toolchain instead of the standard autodetecting toolchain. This can be done by passing the flag `--extra_toolchains=@rules_python//python:autodetecting_toolchain_nonstrict` on the command line or adding it to your bazelrc.
    ```
    This function defines a custom auto detcting Python toolchain that looks for
    a Python 3 interpreter within a repository rule, so that Bazel's sandboxing
    does not restrict the visible installation paths. It then registers an
    appropriate Python toolchain, so that build actions themselves can still be
    sandboxed.
    """
    _config_python3_toolchain(name = name)
    native.register_toolchains("@{}//:toolchain".format(name))

# Note [GHC toolchain files]
# ~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# The GHC distribution includes various files that may be required during
# compilation, or may be referenced by template Haskell code. These files
# need to be tracked by Bazel and declared as inputs to the relevant actions
# to ensure that they are present in the build sandbox.
#
# Surprisingly, builds succeed with little to none of such files declared as
# inputs. In case of the nixpkgs toolchain this is not surprising, as the
# files will all be present in the Nix store. However, files of the bindist
# toolchain are fully tracked by Bazel and one would expect errors due to
# missing files if they are not declared.
#
# The first instance of such an error occurred with the GHC bindist on
# BazelCI [1] and shortly after on the GitHub actions CI pipeline. However,
# only the `lib/settings` file was reported missing. In that instance it was
# sufficient to track `lib/settings` as an explicit build input to avoid this
# error.
#
# However, the issue re-appeared on the Bazel@HEAD CI pipeline [2]. It turns
# out that the failing builds were referencing the `lib/settings` file in a
# different sandbox working directory. I.e. build actions were leaking absolute
# paths to the sandbox working directory in build artifacts. In this case the
# `ghc-paths` package was the root of the issue. It uses a custom Cabal setup
# that hard codes the path to the GHC installation. In the GHC bindist case
# this path lies within the sandbox working directory and may no longer be
# valid in later build actions or at runtime.
#
# To avoid this type of issue we provide a Bazel compatible replacement of the
# `ghc-paths` package in `//tools/ghc-paths`. Refer to
# `//tools/ghc-paths:README.md` and [3] for further information.
#
# [1]: https://github.com/tweag/rules_haskell/issues/1470
# [2]: https://github.com/tweag/rules_haskell/issues/1495
# [3]: https://github.com/tweag/rules_haskell/pull/1508
