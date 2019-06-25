"""Workspace rules (GHC binary distributions)"""

load("@bazel_tools//tools/build_defs/repo:utils.bzl", "patch")
load(
    ":private/workspace_utils.bzl",
    "ghc_is_static",
)

_GHC_DEFAULT_VERSION = "8.6.5"

# Generated with `bazel run @io_tweag_rules_haskell//haskell:gen-ghc-bindist`
# To add a version or architecture, edit the constants in haskell/gen_ghc_bindist.py,
# regenerate the dict and copy it here.
# We’d like to put this dict into its own file,
# but that triggers a bug in Skydoc unfortunately.
GHC_BINDIST = \
    {
        "7.10.3": {
            "darwin_amd64": (
                "https://downloads.haskell.org/~ghc/7.10.3/ghc-7.10.3-x86_64-apple-darwin.tar.xz",
                "b7cad2ea7badb7006621105fbf24b4bd364d2e51c1a75661978d9280d68e83a8",
            ),
            "linux_amd64": (
                "https://downloads.haskell.org/~ghc/7.10.3/ghc-7.10.3-x86_64-deb8-linux.tar.xz",
                "804c75c4635353bf987c1ca120b8531c7bb4957c5b84d29c7adc4894b6fd579d",
            ),
            "windows_amd64": (
                "https://downloads.haskell.org/~ghc/7.10.3/ghc-7.10.3-x86_64-unknown-mingw32.tar.xz",
                "cc7987ca7ffcd8fc8b999ed8f7408300cd9fef156032338fd57d63f577532b81",
            ),
        },
        "8.0.2": {
            "darwin_amd64": (
                "https://downloads.haskell.org/~ghc/8.0.2/ghc-8.0.2-x86_64-apple-darwin.tar.xz",
                "ff50a2df9f002f33b9f09717ebf5ec5a47906b9b65cc57b1f9849f8b2e06788d",
            ),
            "linux_amd64": (
                "https://downloads.haskell.org/~ghc/8.0.2/ghc-8.0.2-x86_64-deb8-linux.tar.xz",
                "5ee68290db00ca0b79d57bc3a5bdce470de9ce9da0b098a7ce6c504605856c8f",
            ),
            "windows_amd64": (
                "https://downloads.haskell.org/~ghc/8.0.2/ghc-8.0.2-x86_64-unknown-mingw32.tar.xz",
                "8c42c1f4af995205b9816a1e97e2752fe758544c1f5fe77958cdcd319c9c2d53",
            ),
        },
        "8.2.2": {
            "darwin_amd64": (
                "https://downloads.haskell.org/~ghc/8.2.2/ghc-8.2.2-x86_64-apple-darwin.tar.xz",
                "f90fcf62f7e0936a6dfc3601cf663729bfe9bbf85097d2d75f0a16f8c2e95c27",
            ),
            "linux_amd64": (
                "https://downloads.haskell.org/~ghc/8.2.2/ghc-8.2.2-x86_64-deb8-linux.tar.xz",
                "48e205c62b9dc1ccf6739a4bc15a71e56dde2f891a9d786a1b115f0286111b2a",
            ),
            "windows_amd64": (
                "https://downloads.haskell.org/~ghc/8.2.2/ghc-8.2.2-x86_64-unknown-mingw32.tar.xz",
                "1e033df2092aa546e763e7be63167720b32df64f76673ea1ce7ae7c9f564b223",
            ),
        },
        "8.4.1": {
            "darwin_amd64": (
                "https://downloads.haskell.org/~ghc/8.4.1/ghc-8.4.1-x86_64-apple-darwin.tar.xz",
                "d774e39f3a0105843efd06709b214ee332c30203e6c5902dd6ed45e36285f9b7",
            ),
            "linux_amd64": (
                "https://downloads.haskell.org/~ghc/8.4.1/ghc-8.4.1-x86_64-deb8-linux.tar.xz",
                "427c77a934b30c3f1de992c38c072afb4323fe6fb30dbac919ca8cb6ae98fbd9",
            ),
            "windows_amd64": (
                "https://downloads.haskell.org/~ghc/8.4.1/ghc-8.4.1-x86_64-unknown-mingw32.tar.xz",
                "328b013fc651d34e075019107e58bb6c8a578f0155cf3ad4557e6f2661b03131",
            ),
        },
        "8.4.2": {
            "darwin_amd64": (
                "https://downloads.haskell.org/~ghc/8.4.2/ghc-8.4.2-x86_64-apple-darwin.tar.xz",
                "87469222042b9ac23f9db216a8d4e5107297bdbbb99df71eb4d9e7208455def2",
            ),
            "linux_amd64": (
                "https://downloads.haskell.org/~ghc/8.4.2/ghc-8.4.2-x86_64-deb8-linux.tar.xz",
                "246f66eb56f4ad0f1c7755502cfc8f9972f2d067dede17e151f6f479c1f76fbd",
            ),
            "windows_amd64": (
                "https://downloads.haskell.org/~ghc/8.4.2/ghc-8.4.2-x86_64-unknown-mingw32.tar.xz",
                "797634aa9812fc6b2084a24ddb4fde44fa83a2f59daea82e0af81ca3dd323fde",
            ),
        },
        "8.4.3": {
            "darwin_amd64": (
                "https://downloads.haskell.org/~ghc/8.4.3/ghc-8.4.3-x86_64-apple-darwin.tar.xz",
                "af0b455f6c46b9802b4b48dad996619cfa27cc6e2bf2ce5532387b4a8c00aa64",
            ),
            "linux_amd64": (
                "https://downloads.haskell.org/~ghc/8.4.3/ghc-8.4.3-x86_64-deb8-linux.tar.xz",
                "30a402c6d4754a6c020e0547f19ae3ac42e907e35349aa932d347f73e421a8e2",
            ),
            "windows_amd64": (
                "https://downloads.haskell.org/~ghc/8.4.3/ghc-8.4.3-x86_64-unknown-mingw32.tar.xz",
                "8a83cfbf9ae84de0443c39c93b931693bdf2a6d4bf163ffb41855f80f4bf883e",
            ),
        },
        "8.4.4": {
            "darwin_amd64": (
                "https://downloads.haskell.org/~ghc/8.4.4/ghc-8.4.4-x86_64-apple-darwin.tar.xz",
                "28dc89ebd231335337c656f4c5ead2ae2a1acc166aafe74a14f084393c5ef03a",
            ),
            "linux_amd64": (
                "https://downloads.haskell.org/~ghc/8.4.4/ghc-8.4.4-x86_64-deb8-linux.tar.xz",
                "4c2a8857f76b7f3e34ecba0b51015d5cb8b767fe5377a7ec477abde10705ab1a",
            ),
            "windows_amd64": (
                "https://downloads.haskell.org/~ghc/8.4.4/ghc-8.4.4-x86_64-unknown-mingw32.tar.xz",
                "da29dbb0f1199611c7d5bb7b0dd6a7426ca98f67dfd6da1526b033cd3830dc05",
            ),
        },
        "8.6.2": {
            "darwin_amd64": (
                "https://downloads.haskell.org/~ghc/8.6.2/ghc-8.6.2-x86_64-apple-darwin.tar.xz",
                "8ec46a25872226dd7e5cf7271e3f3450c05f32144b96e6b9cb44cc4079db50dc",
            ),
            "linux_amd64": (
                "https://downloads.haskell.org/~ghc/8.6.2/ghc-8.6.2-x86_64-deb8-linux.tar.xz",
                "13f96e8b83bb5bb60f955786ff9085744c24927a33be8a17773f84c7c248533a",
            ),
            "windows_amd64": (
                "https://downloads.haskell.org/~ghc/8.6.2/ghc-8.6.2-x86_64-unknown-mingw32.tar.xz",
                "9a398e133cab09ff2610834337355d4e26c35e0665403fb9ff8db79315f74d3d",
            ),
        },
        "8.6.3": {
            "darwin_amd64": (
                "https://downloads.haskell.org/~ghc/8.6.3/ghc-8.6.3-x86_64-apple-darwin.tar.xz",
                "79d069a1a7d74cfdd7ac2a2711c45d3ddc6265b988a0cefa342714b24f997fc1",
            ),
            "linux_amd64": (
                "https://downloads.haskell.org/~ghc/8.6.3/ghc-8.6.3-x86_64-deb8-linux.tar.xz",
                "291ca565374f4d51cc311488581f3279d3167a064fabfd4a6722fe2bd4532fd5",
            ),
            "windows_amd64": (
                "https://downloads.haskell.org/~ghc/8.6.3/ghc-8.6.3-x86_64-unknown-mingw32.tar.xz",
                "2fec383904e5fa79413e9afd328faf9bc700006c8c3d4bcdd8d4f2ccf0f7fa2a",
            ),
        },
        "8.6.4": {
            "darwin_amd64": (
                "https://downloads.haskell.org/~ghc/8.6.4/ghc-8.6.4-x86_64-apple-darwin.tar.xz",
                "cccb58f142fe41b601d73690809f6089f7715b6a50a09aa3d0104176ab4db09e",
            ),
            "linux_amd64": (
                "https://downloads.haskell.org/~ghc/8.6.4/ghc-8.6.4-x86_64-deb8-linux.tar.xz",
                "34ef5fc8ddf2fc32a027180bea5b1c8a81ea840c87faace2977a572188d4b42d",
            ),
            "windows_amd64": (
                "https://downloads.haskell.org/~ghc/8.6.4/ghc-8.6.4-x86_64-unknown-mingw32.tar.xz",
                "e8d021b7a90772fc559862079da20538498d991956d7557b468ca19ddda22a08",
            ),
        },
        "8.6.5": {
            "darwin_amd64": (
                "https://downloads.haskell.org/~ghc/8.6.5/ghc-8.6.5-x86_64-apple-darwin.tar.xz",
                "dfc1bdb1d303a87a8552aa17f5b080e61351f2823c2b99071ec23d0837422169",
            ),
            "linux_amd64": (
                "https://downloads.haskell.org/~ghc/8.6.5/ghc-8.6.5-x86_64-deb8-linux.tar.xz",
                "c419fd0aa9065fe4d2eb9a248e323860c696ddf3859749ca96a84938aee49107",
            ),
            "windows_amd64": (
                "https://downloads.haskell.org/~ghc/8.6.5/ghc-8.6.5-x86_64-unknown-mingw32.tar.xz",
                "457024c6ea43bdce340af428d86319931f267089398b859b00efdfe2fd4ce93f",
            ),
        },
    }

def _execute_fail_loudly(ctx, args):
    """Execute a command and fail loudly if it fails.

    ATTN: All commands have to be cross-compatible between BSD tools and GNU tools,
    because we want to support macOS. Please cross-reference the macOS man-pages.

    Args:
      ctx: Repository rule context.
      args: Command and its arguments.
    """
    eresult = ctx.execute(args, quiet = False)
    if eresult.return_code != 0:
        fail("{0} failed, aborting creation of GHC bindist".format(" ".join(args)))

def _ghc_bindist_impl(ctx):
    # Avoid rule restart by resolving these labels early. See
    # https://github.com/bazelbuild/bazel/blob/master/tools/cpp/lib_cc_configure.bzl#L17.
    ghc_build = ctx.path(Label("@io_tweag_rules_haskell//haskell:ghc.BUILD.tpl"))

    version = ctx.attr.version
    target = ctx.attr.target
    os, _, arch = target.partition("_")
    python_bin = _find_python(ctx)

    if GHC_BINDIST[version].get(target) == None:
        fail("Operating system {0} does not have a bindist for GHC version {1}".format(ctx.os.name, ctx.attr.version))
    else:
        url, sha256 = GHC_BINDIST[version][target]

    bindist_dir = ctx.path(".")  # repo path

    ctx.download_and_extract(
        url = url,
        output = ".",
        sha256 = sha256,
        type = "tar.xz",
        stripPrefix = "ghc-" + version,
    )

    # We apply some patches, if needed.
    patch(ctx)

    # As the patches may touch the package DB we regenerate the cache.
    if len(ctx.attr.patches) > 0:
        _execute_fail_loudly(ctx, ["./bin/ghc-pkg", "recache"])

    # On Windows the bindist already contains the built executables
    if os != "windows":
        # IMPORTANT: all these scripts have to be compatible with BSD tools!

        # bsdcompatible is to work around the differences for -i on BSD sed
        _execute_fail_loudly(ctx, ["sed", "-e", "s/RelocatableBuild = NO/RelocatableBuild = YES/", "-ibsdcompatible", "mk/config.mk.in"])
        _execute_fail_loudly(ctx, ["./configure", "--prefix", bindist_dir.realpath])
        _execute_fail_loudly(ctx, ["make", "install"])
        ctx.file("patch_bins", executable = True, content = r"""#!/usr/bin/env bash
grep --files-with-matches --null {bindist_dir} bin/* | xargs -0 \
    sed -ibsdcompatible \
        -e '2i\
          DISTDIR="$( dirname "$(resolved="$0"; while tmp="$(readlink "$resolved")"; do resolved="$tmp"; done; echo "$resolved")" )/.."' \
        -e 's:{bindist_dir}:$DISTDIR:'
""".format(
            bindist_dir = bindist_dir.realpath,
        ))
        _execute_fail_loudly(ctx, ["./patch_bins"])

    # Generate BUILD file entries describing each prebuilt package.
    # Cannot use //haskell:pkgdb_to_bzl because that's a generated
    # target. ctx.path() only works on source files.
    pkgdb_to_bzl = ctx.path(Label("@io_tweag_rules_haskell//haskell:private/pkgdb_to_bzl.py"))
    result = ctx.execute([
        python_bin,
        pkgdb_to_bzl,
        ctx.attr.name,
        "lib",
    ])
    if result.return_code:
        fail("Error executing pkgdb_to_bzl.py: {stderr}".format(stderr = result.stderr))
    toolchain_libraries = result.stdout
    toolchain = """
{toolchain_libraries}

haskell_toolchain(
    name = "toolchain-impl",
    tools = [":bin"],
    libraries = toolchain_libraries,
    version = "{version}",
    is_static = {is_static},
    compiler_flags = {compiler_flags},
    haddock_flags = {haddock_flags},
    repl_ghci_args = {repl_ghci_args},
    visibility = ["//visibility:public"],
)
    """.format(
        toolchain_libraries = toolchain_libraries,
        version = ctx.attr.version,
        is_static = ghc_is_static(ctx),
        compiler_flags = ctx.attr.compiler_flags,
        haddock_flags = ctx.attr.haddock_flags,
        repl_ghci_args = ctx.attr.repl_ghci_args,
    )
    ctx.template(
        "BUILD",
        ghc_build,
        substitutions = {
            "%{toolchain}": toolchain,
        },
        executable = False,
    )
    ctx.file("WORKSPACE")

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
        "compiler_flags": attr.string_list(),
        "haddock_flags": attr.string_list(),
        "repl_ghci_args": attr.string_list(),
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
    },
)

def _ghc_bindist_toolchain_impl(ctx):
    os, _, arch = ctx.attr.target.partition("_")
    exec_constraints = [{
        "darwin": "@bazel_tools//platforms:osx",
        "linux": "@bazel_tools//platforms:linux",
        "windows": "@bazel_tools//platforms:windows",
    }.get(os)]
    target_constraints = exec_constraints
    ctx.file(
        "BUILD",
        executable = False,
        content = """
toolchain(
    name = "toolchain",
    toolchain_type = "@io_tweag_rules_haskell//haskell:toolchain",
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
        haddock_flags = None,
        repl_ghci_args = None):
    """Create a new repository from binary distributions of GHC. The
    repository exports two targets:

    * a `bin` filegroup containing all GHC commands,
    * a `threaded-rts` CC library.

    These targets are unpacked from a binary distribution specific to your
    platform. Only the platforms that have a "binary package" on the GHC
    [download page](https://www.haskell.org/ghc/) are supported.

    Example:
       In `WORKSPACE` file:

       ```bzl
       load("@io_tweag_rules_haskell//haskell:haskell.bzl", "ghc_bindist")

       # This repository rule creates @ghc repository.
       ghc_bindist(
         name    = "ghc",
         version = "8.2.2",
       )
       ```
    """
    bindist_name = name
    toolchain_name = "{}-toolchain".format(name)

    # Recent GHC versions on Windows contain a bug:
    # https://gitlab.haskell.org/ghc/ghc/issues/16466
    # We work around this by patching the base configuration.
    patches = {
        "8.6.2": ["@io_tweag_rules_haskell//haskell:assets/ghc_8_6_2_win_base.patch"],
        "8.6.4": ["@io_tweag_rules_haskell//haskell:assets/ghc_8_6_4_win_base.patch"],
        "8.6.5": ["@io_tweag_rules_haskell//haskell:assets/ghc_8_6_5_win_base.patch"],
    }.get(version) if target == "windows_amd64" else None

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
        compiler_flags = compiler_flags,
        haddock_flags = haddock_flags,
        repl_ghci_args = repl_ghci_args,
        target = target,
        **extra_attrs
    )
    _ghc_bindist_toolchain(
        name = toolchain_name,
        bindist_name = bindist_name,
        target = target,
    )
    native.register_toolchains("@{}//:toolchain".format(toolchain_name))

def haskell_register_ghc_bindists(
        version,
        compiler_flags = None,
        haddock_flags = None,
        repl_ghci_args = None):
    """Register GHC binary distributions for all platforms as toolchains.

    Toolchains can be used to compile Haskell code. This function
    registers one toolchain for each known binary distribution on all
    platforms of the given GHC version. During the build, one
    toolchain will be selected based on the host and target platforms
    (See [toolchain resolution][toolchain-resolution]).

    [toolchain-resolution]: https://docs.bazel.build/versions/master/toolchains.html#toolchain-resolution

    """
    if not GHC_BINDIST.get(version):
        fail("Binary distribution of GHC {} not available.".format(version))
    for target in GHC_BINDIST[version]:
        ghc_bindist(
            name = "io_tweag_rules_haskell_ghc_{}".format(target),
            target = target,
            version = version,
            compiler_flags = compiler_flags,
            haddock_flags = haddock_flags,
            repl_ghci_args = repl_ghci_args,
        )

def _find_python(repository_ctx):
    python = repository_ctx.which("python3")
    if not python:
        python = repository_ctx.which("python")
        result = repository_ctx.execute([python, "--version"])
        if not result.stdout.startswith("Python 3"):
            fail("rules_haskell requires Python >= 3.3.")
    return python
