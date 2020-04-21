"""Workspace rules (GHC binary distributions)"""

load("@bazel_tools//tools/build_defs/repo:utils.bzl", "patch")
load("@bazel_tools//tools/cpp:lib_cc_configure.bzl", "get_cpu_value")
load("@rules_sh//sh:posix.bzl", "sh_posix_configure")
load(":private/workspace_utils.bzl", "execute_or_fail_loudly")

_GHC_DEFAULT_VERSION = "8.6.5"  # If you change this, change stackage's version
# in the start script (see stackage.org)

# Generated with `bazel run @rules_haskell//haskell:gen-ghc-bindist`
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
        "8.8.1": {
            "darwin_amd64": (
                "https://downloads.haskell.org/~ghc/8.8.1/ghc-8.8.1-x86_64-apple-darwin.tar.xz",
                "38c8917b47c31bedf58c9305dfca3abe198d8d35570366f0773c4e2948bd8abe",
            ),
            "linux_amd64": (
                "https://downloads.haskell.org/~ghc/8.8.1/ghc-8.8.1-x86_64-deb8-linux.tar.xz",
                "fd96eb851971fbc3332bf2fa7821732cfa8b37e5a076a69f6a06f83f0ea7ccc5",
            ),
            "windows_amd64": (
                "https://downloads.haskell.org/~ghc/8.8.1/ghc-8.8.1-x86_64-unknown-mingw32.tar.xz",
                "29e56e6af38017a5a76b2b6995a39d3988fa58131e4b55b62dd317ba7186ac9b",
            ),
        },
        "8.8.2": {
            "darwin_amd64": (
                "https://downloads.haskell.org/~ghc/8.8.2/ghc-8.8.2-x86_64-apple-darwin.tar.xz",
                "25c5c1a70036abf3f22b2b19c10d26adfdb08e8f8574f89d4b2042de5947f990",
            ),
            "linux_amd64": (
                "https://downloads.haskell.org/~ghc/8.8.2/ghc-8.8.2-x86_64-deb8-linux.tar.xz",
                "fbe69652eba75dadb758d00292247d17fb018c29cac5acd79843e56311256c9f",
            ),
            "windows_amd64": (
                "https://downloads.haskell.org/~ghc/8.8.2/ghc-8.8.2-x86_64-unknown-mingw32.tar.xz",
                "e25d9b16ee62cafc7387af2cd021eea676a99cd2c32b83533b016162c63065d9",
            ),
        },
        "8.8.3": {
            "darwin_amd64": (
                "https://downloads.haskell.org/~ghc/8.8.3/ghc-8.8.3-x86_64-apple-darwin.tar.xz",
                "7016de90dd226b06fc79d0759c5d4c83c2ab01d8c678905442c28bd948dbb782",
            ),
            "linux_amd64": (
                "https://downloads.haskell.org/~ghc/8.8.3/ghc-8.8.3-x86_64-deb8-linux.tar.xz",
                "92b9fadc442976968d2c190c14e000d737240a7d721581cda8d8741b7bd402f0",
            ),
            "windows_amd64": (
                "https://downloads.haskell.org/~ghc/8.8.3/ghc-8.8.3-x86_64-unknown-mingw32.tar.xz",
                "e22586762af0911c06e8140f1792e3ca381a3a482a20d67b9054883038b3a422",
            ),
        },
        "8.10.1": {
            "darwin_amd64": (
                "https://downloads.haskell.org/~ghc/8.10.1/ghc-8.10.1-x86_64-apple-darwin.tar.xz",
                "65b1ca361093de4804a7e40b3e68178e1ef720f84f743641ec8d95e56a45b3a8",
            ),
            "linux_amd64": (
                "https://downloads.haskell.org/~ghc/8.10.1/ghc-8.10.1-x86_64-deb9-linux.tar.xz",
                "d1cf7886f27af070f3b7dbe1975a78b43ef2d32b86362cbe953e79464fe70761",
            ),
            "windows_amd64": (
                "https://downloads.haskell.org/~ghc/8.10.1/ghc-8.10.1-x86_64-unknown-mingw32.tar.xz",
                "38a3166ea50cccd5bae7e1680eae3aae2b4ae31b61f82a1d8168fb821f43bd67",
            ),
        },
    }

def _ghc_bindist_impl(ctx):
    # Avoid rule restart by resolving these labels early. See
    # https://github.com/bazelbuild/bazel/blob/master/tools/cpp/lib_cc_configure.bzl#L17.
    ghc_build = ctx.path(Label("@rules_haskell//haskell:ghc.BUILD.tpl"))

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
        execute_or_fail_loudly(ctx, ["./bin/ghc-pkg", "recache"])

    # On Windows the bindist already contains the built executables
    if os != "windows":
        # IMPORTANT: all these scripts have to be compatible with BSD
        # tools! This means that sed -i always takes an argument.
        execute_or_fail_loudly(ctx, ["sed", "-e", "s/RelocatableBuild = NO/RelocatableBuild = YES/", "-i.bak", "mk/config.mk.in"])
        execute_or_fail_loudly(ctx, ["./configure", "--prefix", bindist_dir.realpath])
        execute_or_fail_loudly(ctx, ["make", "install"])
        ctx.file("patch_bins", executable = True, content = r"""#!/usr/bin/env bash
grep --files-with-matches --null {bindist_dir} bin/* | xargs -0 -n1 \
    sed -i.bak \
        -e '2i\
          DISTDIR="$( dirname "$(resolved="$0"; while tmp="$(readlink "$resolved")"; do resolved="$tmp"; done; echo "$resolved")" )/.."' \
        -e 's:{bindist_dir}:$DISTDIR:'
""".format(
            bindist_dir = bindist_dir.realpath,
        ))
        execute_or_fail_loudly(ctx, ["./patch_bins"])

    # The default locale is OS specific.
    if ctx.attr.locale:
        locale = ctx.attr.locale
    else:
        locale = "en_US.UTF-8" if os == "darwin" else "C.UTF-8"

    # Generate BUILD file entries describing each prebuilt package.
    # Cannot use //haskell:pkgdb_to_bzl because that's a generated
    # target. ctx.path() only works on source files.
    pkgdb_to_bzl = ctx.path(Label("@rules_haskell//haskell:private/pkgdb_to_bzl.py"))
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
    locale = "{locale}",
)
    """.format(
        toolchain_libraries = toolchain_libraries,
        version = ctx.attr.version,
        is_static = os == "windows",
        compiler_flags = ctx.attr.compiler_flags,
        haddock_flags = ctx.attr.haddock_flags,
        repl_ghci_args = ctx.attr.repl_ghci_args,
        locale = locale,
    )

    if os == "windows":
        # These libraries cause linking errors on Windows when linking
        # pthreads, due to libwinpthread-1.dll not being loaded.
        execute_or_fail_loudly(ctx, ["rm", "mingw/lib/gcc/x86_64-w64-mingw32/7.2.0/libstdc++.dll.a"])
        execute_or_fail_loudly(ctx, ["rm", "mingw/x86_64-w64-mingw32/lib/libpthread.dll.a"])
        execute_or_fail_loudly(ctx, ["rm", "mingw/x86_64-w64-mingw32/lib/libwinpthread.dll.a"])

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
        "locale": attr.string(
            doc = "Locale that will be set during compiler invocations. Default: C.UTF-8 (en_US.UTF-8 on MacOS)",
            mandatory = False,
        ),
    },
)

def _ghc_bindist_toolchain_impl(ctx):
    os, _, arch = ctx.attr.target.partition("_")
    exec_constraints = [{
        "darwin": "@platforms//os:osx",
        "linux": "@platforms//os:linux",
        "windows": "@platforms//os:windows",
    }.get(os)]
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
        haddock_flags = None,
        repl_ghci_args = None,
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
    """
    bindist_name = name
    toolchain_name = "{}-toolchain".format(name)

    # Recent GHC versions on Windows contain a bug:
    # https://gitlab.haskell.org/ghc/ghc/issues/16466
    # We work around this by patching the base configuration.
    patches = {
        "8.6.2": ["@rules_haskell//haskell:assets/ghc_8_6_2_win_base.patch"],
        "8.6.4": ["@rules_haskell//haskell:assets/ghc_8_6_4_win_base.patch"],
        "8.6.5": ["@rules_haskell//haskell:assets/ghc_8_6_5_win_base.patch"],
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
        haddock_flags = None,
        repl_ghci_args = None,
        locale = None):
    """Register GHC binary distributions for all platforms as toolchains.

    Toolchains can be used to compile Haskell code. This function
    registers one toolchain for each known binary distribution on all
    platforms of the given GHC version. During the build, one
    toolchain will be selected based on the host and target platforms
    (See [toolchain resolution][toolchain-resolution]).

    [toolchain-resolution]: https://docs.bazel.build/versions/master/toolchains.html#toolchain-resolution

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
            haddock_flags = haddock_flags,
            repl_ghci_args = repl_ghci_args,
            locale = locale,
        )
    local_sh_posix_repo_name = "rules_haskell_sh_posix_local"
    if local_sh_posix_repo_name not in native.existing_rules():
        sh_posix_configure(name = local_sh_posix_repo_name)
    local_python_repo_name = "rules_haskell_python_local"
    if local_python_repo_name not in native.existing_rules():
        _configure_python3_toolchain(name = local_python_repo_name)

def _find_python(repository_ctx):
    python = repository_ctx.which("python3")
    if not python:
        python = repository_ctx.which("python")
        result = repository_ctx.execute([python, "--version"])
        if not result.stdout.startswith("Python 3"):
            fail("rules_haskell requires Python >= 3.3.")
    return python

def _configure_python3_toolchain_impl(repository_ctx):
    cpu = get_cpu_value(repository_ctx)
    python3_path = _find_python(repository_ctx)
    repository_ctx.file("BUILD.bazel", executable = False, content = """
load(
    "@bazel_tools//tools/python:toolchain.bzl",
    "py_runtime_pair",
)
py_runtime(
    name = "python3_runtime",
    interpreter_path = "{python3}",
    python_version = "PY3",
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
