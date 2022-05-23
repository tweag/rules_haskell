"""Workspace rules (GHC binary distributions)"""

load("@bazel_skylib//lib:paths.bzl", "paths")
load("@bazel_tools//tools/build_defs/repo:utils.bzl", "patch")
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
    "execute_or_fail_loudly",
    "find_python",
    "resolve_labels",
)
load(":private/validate_attrs.bzl", "check_deprecated_attribute_usage")
load(":private/ghc_bindist_generated.bzl", "GHC_BINDIST")

# If you change this, change stackage's version in the start script
# (see stackage.org).
_GHC_DEFAULT_VERSION = "8.10.7"

GHC_BINDIST_STRIP_PREFIX = \
    {
        "9.2.1": {
            "darwin_amd64": "ghc-9.2.1-x86_64-apple-darwin",
            "windows_amd64": "ghc-9.2.1-x86_64-unknown-mingw32",
        },
        "9.0.2": {
            "windows_amd64": "ghc-9.0.2-x86_64-unknown-mingw32",
        },
        "9.0.1": {
            "windows_amd64": "ghc-9.0.1-x86_64-unknown-mingw32",
        },
    }

GHC_BINDIST_LIBDIR = \
    {
        "9.2.1": {
            "darwin_amd64": "lib/lib",
        },
    }

GHC_BINDIST_DOCDIR = \
    {
        "9.2.1": {
            "windows_amd64": "docs",
        },
        "9.0.2": {
            "windows_amd64": "docs",
        },
        "9.0.1": {
            "windows_amd64": "docs",
        },
    }

def _ghc_bindist_impl(ctx):
    filepaths = resolve_labels(ctx, [
        "@rules_haskell//haskell:ghc.BUILD.tpl",
        "@rules_haskell//haskell:private/pkgdb_to_bzl.py",
    ])
    version = ctx.attr.version
    target = ctx.attr.target
    os, _, arch = target.partition("_")

    if GHC_BINDIST[version].get(target) == None:
        fail("Operating system {0} does not have a bindist for GHC version {1}".format(ctx.os.name, ctx.attr.version))
    else:
        url, sha256 = GHC_BINDIST[version][target]

    bindist_dir = ctx.path(".")  # repo path

    # The Windows bindist is ready to use after unpacking, so we can unpack it
    # straight into the repository root. However, the Linux bindist requires a
    # `./configure && make install` which will install the final bindist into
    # the destination directory. We unpack the distribution into a
    # sub-directory in order to cleanly separate the usable installation from
    # the raw distribution.
    unpack_dir = "bindist_unpacked" if os != "windows" else ""

    stripPrefix = "ghc-" + version
    if GHC_BINDIST_STRIP_PREFIX.get(version) != None and GHC_BINDIST_STRIP_PREFIX[version].get(target) != None:
        stripPrefix = GHC_BINDIST_STRIP_PREFIX[version][target]

    ctx.download_and_extract(
        url = url,
        output = unpack_dir,
        sha256 = sha256,
        type = "tar.xz",
        stripPrefix = stripPrefix,
    )

    if os == "windows":
        # These libraries cause linking errors on Windows when linking
        # pthreads, due to libwinpthread-1.dll not being loaded.
        dll_a_libs = ["libstdc++.dll.a", "libpthread.dll.a", "libwinpthread.dll.a"]

        # Similarly causes loading issues with template Haskell. E.g.
        #
        #   ghc.exe: panic! (the 'impossible' happened)
        #     (GHC version 8.6.5 for x86_64-unknown-mingw32):
        #      loadArchive "C:\\Users\\runneradmin\\_bazel_runneradmin\\minshlu6\\external\\rules_haskell_ghc_windows_amd64\\mingw\\lib\\libz.dll.a": failed
        #
        #   Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug
        #
        #   ghc.exe: Could not load `zlib1.dll'. Reason: addDLL: zlib1.dll or dependencies not loaded. (Win32 error 126)
        #
        # on //tests/haddock:haddock-lib-b.
        dll_a_libs.append("libz.dll.a")

        # It's hard to guesss the paths of these libraries, so we have to use
        # dir to recursively find them.
        for lib in dll_a_libs:
            result = ctx.execute(["cmd", "/c", "dir", "/s", "/b", lib])
            for path in result.stdout.splitlines():
                ctx.execute(["cmd", "/c", "del", path.strip()], working_directory = unpack_dir)

    # We apply some patches, if needed.
    patch_args = list(ctx.attr.patch_args)
    if unpack_dir:
        patch_args.extend(["-d", unpack_dir])
    patch(ctx, patch_args = patch_args)

    # On Windows the bindist already contains the built executables
    if os != "windows":
        # IMPORTANT: all these scripts have to be compatible with BSD
        # tools! This means that sed -i always takes an argument.
        execute_or_fail_loudly(ctx, ["sed", "-e", "s/RelocatableBuild = NO/RelocatableBuild = YES/", "-i.bak", "mk/config.mk.in"], working_directory = unpack_dir)
        execute_or_fail_loudly(ctx, ["rm", "-f", "mk/config.mk.in.bak"], working_directory = unpack_dir)
        execute_or_fail_loudly(ctx, ["./configure", "--prefix", bindist_dir.realpath], working_directory = unpack_dir)
        make_loc = ctx.which("make")
        if not make_loc:
            fail("It looks like the build-essential package might be missing, because there is no make in PATH.  Are the required dependencies installed?  https://rules-haskell.readthedocs.io/en/latest/haskell.html#before-you-begin")

        if version == "9.2.1":
            # Necessary for deterministic builds on macOS. See
            # https://gitlab.haskell.org/ghc/ghc/-/issues/19963
            ctx.file("{}/mk/relpath.sh".format(unpack_dir), ctx.read(ctx.path(ctx.attr._relpath_script)), executable = False, legacy_utf8 = False)
            execute_or_fail_loudly(ctx, ["chmod", "+x", "mk/relpath.sh"], working_directory = unpack_dir)

        execute_or_fail_loudly(
            ctx,
            ["make", "install"],
            # Necessary for deterministic builds on macOS. See
            # https://blog.conan.io/2019/09/02/Deterministic-builds-with-C-C++.html.
            # The proper fix is for the GHC bindist to always use ar
            # and never use libtool, which has a -D flag for
            # deterministic builds that works better than
            # ZERO_AR_DATE. See
            # https://source.chromium.org/chromium/chromium/src/+/62848c8d298690e086e49a9832278ff56b6976b5.
            environment = {"ZERO_AR_DATE": "1"},
            working_directory = unpack_dir,
        )
        ctx.file(paths.join(unpack_dir, "patch_bins"), executable = True, content = r"""#!/usr/bin/env bash
find bin -type f -print0 | xargs -0 \
grep --files-with-matches --null {bindist_dir} | xargs -0 -n1 \
    sed -i.bak \
        -e '2i\
DISTDIR="$( dirname "$(resolved="$0"; cd "$(dirname "$resolved")"; while tmp="$(readlink "$(basename "$resolved")")"; do resolved="$tmp"; cd "$(dirname "$resolved")"; done; echo "$PWD/$(basename "$resolved")")" )/.."' \
        -e 's:{bindist_dir}:$DISTDIR:'
find bin -type f -print0 | xargs -0 \
grep --files-with-matches --null {bindist_dir} | xargs -0 -n1 \
rm -f
""".format(
            bindist_dir = bindist_dir.realpath,
        ))
        execute_or_fail_loudly(ctx, [paths.join(".", unpack_dir, "patch_bins")])

    # As the patches may touch the package DB we regenerate the cache.
    if len(ctx.attr.patches) > 0:
        execute_or_fail_loudly(ctx, ["./bin/ghc-pkg", "recache"])

    libdir = "lib"
    if GHC_BINDIST_LIBDIR.get(version) != None and GHC_BINDIST_LIBDIR[version].get(target) != None:
        libdir = GHC_BINDIST_LIBDIR[version][target]

    docdir = "doc"
    if GHC_BINDIST_DOCDIR.get(version) != None and GHC_BINDIST_DOCDIR[version].get(target) != None:
        docdir = GHC_BINDIST_DOCDIR[version][target]

    toolchain_libraries = pkgdb_to_bzl(ctx, filepaths, libdir)["file_content"]
    locale = ctx.attr.locale or ("en_US.UTF-8" if os == "darwin" else "C.UTF-8")
    toolchain = define_rule(
        "haskell_toolchain",
        name = "toolchain-impl",
        tools = [":bin"],
        libraries = "toolchain_libraries",
        # See Note [GHC toolchain files]
        libdir = [":lib"],
        docdir = [":{}".format(docdir)],
        version = repr(ctx.attr.version),
        static_runtime = os == "windows",
        fully_static_link = False,  # XXX not yet supported for bindists.
        ghcopts = ctx.attr.ghcopts,
        haddock_flags = ctx.attr.haddock_flags,
        repl_ghci_args = ctx.attr.repl_ghci_args,
        cabalopts = ctx.attr.cabalopts,
        locale = repr(locale),
    )
    ctx.template(
        "BUILD",
        filepaths["@rules_haskell//haskell:ghc.BUILD.tpl"],
        substitutions = {
            "%{toolchain_libraries}": toolchain_libraries,
            "%{toolchain}": toolchain,
            "%{docdir}": docdir,
        },
        executable = False,
    )

_ghc_bindist = repository_rule(
    _ghc_bindist_impl,
    local = False,
    attrs = {
        "version": attr.string(
            default = _GHC_DEFAULT_VERSION,
            values = GHC_BINDIST.keys(),
            doc = "The desired GHC version",
        ),
        "target": attr.string(),
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

def _ghc_bindist_toolchain_impl(ctx):
    os, _, arch = ctx.attr.target.partition("_")
    os_constraint = {
        "darwin": "osx",
        "linux": "linux",
        "windows": "windows",
    }.get(os)
    cpu_constraint = {
        "amd64": "x86_64",
        "arm64": "arm64",
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

_ghc_bindist_toolchain = repository_rule(
    _ghc_bindist_toolchain_impl,
    local = False,
    attrs = {
        "bindist_name": attr.string(),
        "target": attr.string(),
    },
)

def ghc_bindist(
        name,
        version,
        target,
        compiler_flags = None,
        ghcopts = None,
        haddock_flags = None,
        repl_ghci_args = None,
        cabalopts = None,
        locale = None):
    """Create a new repository from binary distributions of GHC.

    The repository exports two targets:

    * a `bin` filegroup containing all GHC commands,
    * a `threaded-rts` CC library.

    These targets are unpacked from a binary distribution specific to your
    platform. Only the platforms that have a "binary package" on the GHC
    [download page](https://www.haskell.org/ghc/) are supported.

    ### Examples

       In `WORKSPACE` file:

       ```bzl
       load("@rules_haskell//haskell:ghc_bindist.bzl", "ghc_bindist")

       # This repository rule creates @ghc repository.
       ghc_bindist(
         name    = "ghc",
         version = "8.2.2",
       )
       ```

    Args:
      name: A unique name for the repository.
      version: The desired GHC version.
      compiler_flags: [see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-compiler_flags)
      ghcopts: [see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-ghcopts)
      haddock_flags: [see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-haddock_flags)
      repl_ghci_args: [see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-repl_ghci_args)
      cabalopts: [see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-cabalopts)
      locale: [see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-locale)

    """
    ghcopts = check_deprecated_attribute_usage(
        old_attr_name = "compiler_flags",
        old_attr_value = compiler_flags,
        new_attr_name = "ghcopts",
        new_attr_value = ghcopts,
    )

    bindist_name = name
    toolchain_name = "{}-toolchain".format(name)

    # Recent GHC versions on Windows contain a bug:
    # https://gitlab.haskell.org/ghc/ghc/issues/16466
    # We work around this by patching the base configuration.
    patches = None
    if target == "windows_amd64":
        patches = {
            "8.6.2": ["@rules_haskell//haskell:assets/ghc_8_6_2_win_base.patch"],
            "8.6.4": ["@rules_haskell//haskell:assets/ghc_8_6_4_win_base.patch"],
            "8.6.5": ["@rules_haskell//haskell:assets/ghc_8_6_5_win_base.patch"],
            "8.8.1": ["@rules_haskell//haskell:assets/ghc_8_8_1_win_base.patch"],
            "8.8.2": ["@rules_haskell//haskell:assets/ghc_8_8_2_win_base.patch"],
            "8.8.3": ["@rules_haskell//haskell:assets/ghc_8_8_3_win_base.patch"],
            "8.8.4": ["@rules_haskell//haskell:assets/ghc_8_8_4_win_base.patch"],
            "9.0.1": ["@rules_haskell//haskell:assets/ghc_9_0_1_win.patch"],
            "9.0.2": ["@rules_haskell//haskell:assets/ghc_9_0_2_win.patch"],
            "9.2.1": ["@rules_haskell//haskell:assets/ghc_9_2_1_win.patch"],
        }.get(version)

    if target == "darwin_amd64":
        patches = {
            # Patch for https://gitlab.haskell.org/ghc/ghc/-/issues/19963
            "9.2.1": ["@rules_haskell//haskell:assets/ghc_9_2_1_mac.patch"],
        }.get(version)

    extra_attrs = {"patches": patches, "patch_args": ["-p0"]} if patches else {}

    # We want the toolchain definition to be tucked away in a separate
    # repository, that way `bazel build //...` will not match it (and
    # e.g. build the Windows toolchain even on Linux). At the same
    # time, we don't want the definition in the bindist repository,
    # because then we need to download the bindist first before we can
    # see the toolchain definition. The solution is to add the
    # toolchain definition in its own special repository.
    _ghc_bindist(
        name = bindist_name,
        version = version,
        ghcopts = ghcopts,
        haddock_flags = haddock_flags,
        repl_ghci_args = repl_ghci_args,
        cabalopts = cabalopts,
        target = target,
        locale = locale,
        **extra_attrs
    )
    _ghc_bindist_toolchain(
        name = toolchain_name,
        bindist_name = bindist_name,
        target = target,
    )
    native.register_toolchains("@{}//:toolchain".format(toolchain_name))

def haskell_register_ghc_bindists(
        version = None,
        compiler_flags = None,
        ghcopts = None,
        haddock_flags = None,
        repl_ghci_args = None,
        cabalopts = None,
        locale = None):
    """ Register GHC binary distributions for all platforms as toolchains.

    See [rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains).

    Args:
      version: [see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-version)
      compiler_flags: [see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-compiler_flags)
      ghcopts: [see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-ghcopts)
      haddock_flags: [see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-haddock_flags)
      repl_ghci_args: [see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-repl_ghci_args)
      cabalopts: [see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-cabalopts)
      locale: [see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-locale)
    """
    version = version or _GHC_DEFAULT_VERSION
    if not GHC_BINDIST.get(version):
        fail("Binary distribution of GHC {} not available.".format(version))
    for target in GHC_BINDIST[version]:
        ghc_bindist(
            name = "rules_haskell_ghc_{}".format(target),
            target = target,
            version = version,
            compiler_flags = compiler_flags,
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

    This function defins a custom auto detcting Python toolchain that looks for
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
