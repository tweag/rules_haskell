"""Workspace rules (GHC binary distributions)"""

load("@bazel_skylib//lib:paths.bzl", "paths")
load("@bazel_tools//tools/build_defs/repo:utils.bzl", "patch")
load("@bazel_tools//tools/cpp:lib_cc_configure.bzl", "get_cpu_value")
load("@rules_cc//cc:find_cc_toolchain.bzl", "CC_TOOLCHAIN_TYPE")
load("@rules_sh//sh:posix.bzl", "sh_posix_configure")
load("//haskell:ghc.bzl", "DEFAULT_GHC_VERSION")
load(":private/bazel_platforms.bzl", "bazel_platforms")
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

_GHC_DEFAULT_VERSION = DEFAULT_GHC_VERSION

GHC_BINDIST_STRIP_PREFIX = \
    {
    }

GHC_BINDIST_LIBDIR = \
    {
    }

GHC_BINDIST_DOCDIR = \
    {
    }

LOCAL_PYTHON_REPO_NAME = "rules_haskell_python_local"

def _split_version(version):
    vs = version.split(".")
    if len(vs) != 3:
        fail("GHC version should be a triple: {}".format(version))
    return (int(vs[0]), int(vs[1]), int(vs[2]))

def _load_bindists(mctx):
    bindist_json = mctx.read(Label("@rules_haskell//haskell:private/ghc_bindist_generated.json"))
    return json.decode(bindist_json)

def bindist_info_for_version(ctx, version):
    GHC_BINDIST = _load_bindists(ctx)

    bindist = GHC_BINDIST.get(version or _GHC_DEFAULT_VERSION)

    if bindist == None:
        fail("Binary distribution of GHC {} not available.".format(version))

    return bindist

def _ghc_bindist_impl(ctx):
    filepaths = resolve_labels(ctx, [
        "@rules_haskell//haskell:ghc.BUILD.tpl",
        "@rules_haskell//haskell:private/pkgdb_to_bzl.py",
    ])
    version = ctx.attr.version
    version_tuple = _split_version(version)
    target = ctx.attr.target
    os, _, arch = target.partition("_")

    bindist = bindist_info_for_version(ctx, version)

    if target not in bindist:
        fail("Operating system {0} does not have a bindist for GHC version {1}".format(ctx.os.name, ctx.attr.version))
    else:
        bindists = bindist[target]
        dist = ctx.attr.dist.get(target)
        if dist:
            filtered_bindists = [bindist for bindist in bindists if bindist["dist"] == dist]
            if not filtered_bindists:
                fail("no GHC bindist found with specified `dist` of `{}`\n  available dists: {}".format(
                    dist,
                    ", ".join(sorted([bindist["dist"] for bindist in bindists])),
                ))
            bindists = filtered_bindists

        variant = ctx.attr.variant.get(target)
        if variant:
            filtered_bindists = [bindist for bindist in bindists if bindist.get("variant") == variant]
            if not filtered_bindists:
                fail("no GHC bindist found with specified `variant` of `{}`\n  available dists{}: {}".format(
                    variant,
                    " (where `dist` == {})".format(dist) if dist else "",
                    ", ".join(sorted([bindist.get("variant", "(none)") for bindist in bindists])),
                ))
            bindists = filtered_bindists

        if len(bindists) > 1:
            dists = [bindist["dist"] for bindist in bindists]

            if os.startswith("linux"):
                # for Linux, we use debian dists by default
                debian_dists = sorted([int(d[3:]) for d in dists if d.startswith("deb")])

                if debian_dists:
                    # prefer the oldest version by default
                    deb_version = "deb{}".format(debian_dists[0])

                    bindists = [bindist for bindist in bindists if bindist["dist"] == deb_version]

            if len(bindists) > 1:
                # prefer the one without a variant, but fail if ambiguous
                dists = [bindist for bindist in bindists if "variant" not in bindist]
                if len(dists) > 1:
                    # we should never get here, for a given version, architecture and dist there can only be one
                    # tarball without a variant
                    fail("multiple non-variant entries for GHC {} {}:\n\n{}".format(version, target, bindists))
                if not dists:
                    fail("multiple variant entries for GHC {} {}:\n\n{}\n\nSelect one explicitly using `variant = 'xyz'`".format(version, target, bindists))

        bindist = bindists[0]

        url = bindist["url"]
        sha256 = bindist["sha256"]

    bindist_dir = ctx.path(".")  # repo path

    # The Windows bindist is ready to use after unpacking, so we can unpack it
    # straight into the repository root. However, the Linux bindist requires a
    # `./configure && make install` which will install the final bindist into
    # the destination directory. We unpack the distribution into a
    # sub-directory in order to cleanly separate the usable installation from
    # the raw distribution.
    unpack_dir = "bindist_unpacked" if os != "windows" else ""

    strip_prefix = "ghc-" + version
    if GHC_BINDIST_STRIP_PREFIX.get(version) != None and GHC_BINDIST_STRIP_PREFIX[version].get(target) != None:
        strip_prefix = GHC_BINDIST_STRIP_PREFIX[version][target]
    else:
        arch_suffix = {"arm64": "aarch64", "amd64": "x86_64"}.get(arch)

        if os == "windows" and version_tuple >= (9, 0, 1):
            strip_prefix += "-{}-unknown-mingw32".format(arch_suffix)
        elif os == "darwin" and version_tuple >= (9, 0, 2):
            strip_prefix += "-{}-apple-darwin".format(arch_suffix)
        elif os == "linux" and version_tuple >= (9, 4, 1):
            strip_prefix += "-{}-unknown-linux".format(arch_suffix)

    ctx.download_and_extract(
        url = url,
        output = unpack_dir,
        sha256 = sha256,
        type = "tar.xz",
        stripPrefix = strip_prefix,
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

        # Similarly causes loading issues when using libc++ with clang on GHC >= 9.4
        dll_a_libs.append("libc++.dll.a")

        # It's hard to guesss the paths of these libraries, so we have to use
        # dir to recursively find them.
        for lib in dll_a_libs:
            result = ctx.execute(["cmd", "/c", "dir", "/s", "/b", lib])
            for path in result.stdout.splitlines():
                ctx.execute(["cmd", "/c", "del", path.strip()], working_directory = unpack_dir)

        # Recent GHC versions need to be patched to
        # work around https://gitlab.haskell.org/ghc/ghc/-/issues/23476 where ghc-pkg resolves
        # haddock-html and haddock-interfaces to non-existing paths, because the ${pkgroot}
        # relative path refers to a directory in the parent directory of the distribution.
        if version_tuple >= (9, 0, 1):
            ctx.report_progress("fixing package-db paths")
            pkgdb = execute_or_fail_loudly(ctx, [paths.join("bin", "ghc"), "--print-global-package-db"]).stdout.splitlines()[0]

            config_files = execute_or_fail_loudly(ctx, ["where", "/r", pkgdb.strip(), "*.conf"]).stdout

            repo_root = str(ctx.path("."))

            for conf in config_files.splitlines():
                # `conf` is always absolute, relativize it to the repo_root
                conf_path = str(ctx.path(conf.strip()))
                conf_path = conf_path[len(repo_root) + 1:]

                config = ctx.read(conf_path)
                if "${pkgroot}/../../" in config:
                    ctx.file(conf_path, content = config.replace("${pkgroot}/../../", "${pkgroot}/../"))

    # We apply some patches, if needed.
    patch_args = list(ctx.attr.patch_args)
    if unpack_dir:
        patch_args.extend(["-d", unpack_dir])
    patch(ctx, patch_args = patch_args)

    is_hadrian_dist = ctx.path(unpack_dir).get_child("config.mk.in").exists

    # On Windows the bindist already contains the built executables
    if os != "windows":
        # IMPORTANT: all these scripts have to be compatible with BSD
        # tools! This means that sed -i always takes an argument.

        if is_hadrian_dist:
            ctx.file(paths.join(unpack_dir, "relocatable.mk"), content = """
RelocatableBuild := YES
include Makefile""")
            make_args = ["-f", "relocatable.mk"]
        else:
            make_args = []

            execute_or_fail_loudly(ctx, ["sed", "-e", "s/RelocatableBuild = NO/RelocatableBuild = YES/", "-i.bak", "mk/config.mk.in"], working_directory = unpack_dir)
            execute_or_fail_loudly(ctx, ["rm", "-f", "mk/config.mk.in.bak"], working_directory = unpack_dir)

        execute_or_fail_loudly(ctx, ["./configure", "--prefix", bindist_dir.realpath], working_directory = unpack_dir)

        make_loc = ctx.which("make")
        if not make_loc:
            fail("It looks like the build-essential package might be missing, because there is no make in PATH.  Are the required dependencies installed?  https://rules-haskell.readthedocs.io/en/latest/haskell.html#before-you-begin")

        if version_tuple >= (9, 2, 1):
            # Necessary for deterministic builds on macOS. See
            # https://gitlab.haskell.org/ghc/ghc/-/issues/19963
            ctx.file("{}/mk/relpath.sh".format(unpack_dir), ctx.read(ctx.path(ctx.attr._relpath_script)), executable = False, legacy_utf8 = False)
            execute_or_fail_loudly(ctx, ["chmod", "+x", "mk/relpath.sh"], working_directory = unpack_dir)

        execute_or_fail_loudly(
            ctx,
            ["make", "install"] + make_args,
            # Necessary for deterministic builds on macOS. See
            # https://blog.conan.io/2019/09/02/Deterministic-builds-with-C-C++.html.
            # The proper fix is for the GHC bindist to always use ar
            # and never use libtool, which has a -D flag for
            # deterministic builds that works better than
            # ZERO_AR_DATE. See
            # https://source.chromium.org/chromium/chromium/src/+/62848c8d298690e086e49a9832278ff56b6976b5.
            environment = {"ZERO_AR_DATE": "1"},
            working_directory = unpack_dir,
            # use a big timeout because copying GHC is slow (1.5G)
            timeout = 30 * 60,
        )

        if not is_hadrian_dist:
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
    elif os == "darwin" and version_tuple >= (9, 0, 2) and version_tuple < (9, 10, 1):
        libdir = "lib/lib"
    elif os == "linux" and version_tuple >= (9, 4, 1) and version_tuple < (9, 10, 1):
        libdir = "lib/lib"

    docdir = "doc"
    if GHC_BINDIST_DOCDIR.get(version) != None and GHC_BINDIST_DOCDIR[version].get(target) != None:
        docdir = GHC_BINDIST_DOCDIR[version][target]
    elif os == "windows" and version_tuple >= (9, 0, 1) and version_tuple < (9, 4, 1):
        docdir = "docs"

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

    is_clang = ctx.path(paths.join(unpack_dir, "mingw", "bin", "clang.exe")).exists

    ctx.template(
        "BUILD",
        filepaths["@rules_haskell//haskell:ghc.BUILD.tpl"],
        substitutions = {
            "%{toolchain_libraries}": toolchain_libraries,
            "%{toolchain}": toolchain,
            "%{docdir}": docdir,
            "%{is_clang}": str(is_clang),
        },
        executable = False,
    )

_ghc_bindist = repository_rule(
    _ghc_bindist_impl,
    local = False,
    attrs = {
        "version": attr.string(
            default = _GHC_DEFAULT_VERSION,
            doc = "The desired GHC version",
        ),
        "target": attr.string(),
        "ghcopts": attr.string_list(),
        "haddock_flags": attr.string_list(),
        "repl_ghci_args": attr.string_list(),
        "cabalopts": attr.string_list(),
        "dist": attr.string_dict(
            doc = "Select a specific `dist` of a GHC binary tarball (e.g. deb10, alpine312) for a platform",
        ),
        "variant": attr.string_dict(
            doc = "Select a specific `variant` of a GHC binary tarball (e.g. dwarf, native_int) for a platform",
        ),
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

def ghc_bindist_toolchain_declaration(target, bindist_name, toolchain_name):
    os, _, arch = target.partition("_")
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
    return """
toolchain(
    name = "{toolchain_name}",
    toolchain_type = "@rules_haskell//haskell:toolchain",
    toolchain = "@{bindist_name}//:toolchain-impl",
    exec_compatible_with = {exec_constraints},
    target_compatible_with = {target_constraints},
)
""".format(
        toolchain_name = toolchain_name,
        bindist_name = bindist_name,
        exec_constraints = exec_constraints,
        target_constraints = target_constraints,
    )

def _ghc_bindist_toolchain_impl(ctx):
    ctx.file(
        "BUILD",
        executable = False,
        content = ghc_bindist_toolchain_declaration(
            target = ctx.attr.target,
            bindist_name = ctx.attr.bindist_name,
            toolchain_name = "toolchain",
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

def _windows_cc_toolchain_impl(repository_ctx):
    repository_ctx.file("BUILD.bazel", executable = False, content = """
toolchain(
    name = "windows_cc_toolchain",
    exec_compatible_with = [
      "@platforms//os:windows",
      "@platforms//cpu:x86_64"
    ],
    target_compatible_with = [
      "@platforms//os:windows",
      "@platforms//cpu:x86_64"
    ],
    toolchain = "@{name}//:cc-compiler-mingw64",
    toolchain_type = "{cc_toolchain}",
)
""".format(
        name = repository_ctx.attr.bindist_name,
        cc_toolchain = CC_TOOLCHAIN_TYPE,
    ))

_windows_cc_toolchain = repository_rule(
    _windows_cc_toolchain_impl,
    local = False,
    attrs = {
        "bindist_name": attr.string(),
    },
)

# Toolchains declarations for bindists used by the `haskell_toolchains` module
# extension to register all the haskell toolchains in the same BUILD file
def ghc_bindists_toolchain_declarations(mctx, version):
    version = version or _GHC_DEFAULT_VERSION

    bindist = bindist_info_for_version(mctx, version)

    return [
        ghc_bindist_toolchain_declaration(
            target = target,
            bindist_name = "rules_haskell_ghc_{}".format(target),
            toolchain_name = "{}",
        )
        for target in bindist
    ]

def ghc_bindist(
        name,
        version,
        target,
        compiler_flags = None,
        ghcopts = None,
        haddock_flags = None,
        repl_ghci_args = None,
        cabalopts = None,
        locale = None,
        register = True,
        **kwargs):
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
      ghcopts: [see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-ghcopts)
      haddock_flags: [see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-haddock_flags)
      repl_ghci_args: [see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-repl_ghci_args)
      cabalopts: [see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-cabalopts)
      locale: [see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-locale)
      register: Whether to register the toolchains (must be set to False if bzlmod is enabled)
    """
    if compiler_flags:
        fail("`compiler_flags` argument was removed, use `ghcopts` instead")

    bindist_name = name
    toolchain_name = "{}-toolchain".format(name)

    patches = None
    if target == "darwin_amd64":
        patches = {
            # Patch for https://gitlab.haskell.org/ghc/ghc/-/issues/19963
            "9.2.1": ["@rules_haskell//haskell:assets/ghc_9_2_1_mac.patch"],
        }.get(version)

    extra_attrs = {"patches": patches, "patch_args": ["-p0"]} if patches else {}
    extra_attrs.update(kwargs)

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
    if register:
        native.register_toolchains("@{}//:toolchain".format(toolchain_name))
    if target == "windows_amd64":
        cc_toolchain_repo_name = "{}_cc_toolchain".format(bindist_name)
        _windows_cc_toolchain(name = cc_toolchain_repo_name, bindist_name = bindist_name)
        if register:
            native.register_toolchains("@{}//:windows_cc_toolchain".format(cc_toolchain_repo_name))

_GHC_AVAILABLE_TARGETS = [
    "darwin_amd64",
    "darwin_arm64",
    "linux_amd64",
    "linux_arm64",
    "windows_amd64",
]

def haskell_register_ghc_bindists(
        version = None,
        compiler_flags = None,  # TODO remove
        ghcopts = None,
        haddock_flags = None,
        repl_ghci_args = None,
        cabalopts = None,
        locale = None,
        register = True,
        targets = _GHC_AVAILABLE_TARGETS,
        **kwargs):
    """ Register GHC binary distributions for all platforms as toolchains.

    See [rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains).

    Args:
      version: [see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-version)
      ghcopts: [see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-ghcopts)
      haddock_flags: [see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-haddock_flags)
      repl_ghci_args: [see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-repl_ghci_args)
      cabalopts: [see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-cabalopts)
      locale: [see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-locale)
      register: Whether to register the toolchains (must be set to False if bzlmod is enabled)
      targets: A list of target platforms to generate bindists for, e.g. `["linux_amd64", "windows_amd64"]` (default: all)
    """
    if compiler_flags:
        fail("`compiler_flags` argument was removed, use `ghcopts` instead")

    version = version or _GHC_DEFAULT_VERSION

    for target in targets:
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
            register = register,
            **kwargs
        )
    local_sh_posix_repo_name = "rules_haskell_sh_posix_local"
    if local_sh_posix_repo_name not in native.existing_rules():
        sh_posix_configure(
            name = local_sh_posix_repo_name,
            register = register,
        )
    if LOCAL_PYTHON_REPO_NAME not in native.existing_rules():
        configure_python3_toolchain(name = LOCAL_PYTHON_REPO_NAME, register = register)

def _configure_python3_toolchain_impl(repository_ctx):
    os_cpu = get_cpu_value(repository_ctx)
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
        "@platforms//cpu:{cpu}",
        "@platforms//os:{os}",
    ],
    target_compatible_with = [
        "@platforms//cpu:{cpu}",
        "@platforms//os:{os}",
    ],
)
""".format(
        python3 = python3_path,
        os = bazel_platforms.get_os(os_cpu),
        cpu = bazel_platforms.get_cpu(os_cpu),
        stub_shebang = stub_shebang,
    ))

_config_python3_toolchain = repository_rule(
    _configure_python3_toolchain_impl,
    configure = True,
    environ = ["PATH"],
)

def configure_python3_toolchain(name, register = True):
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

    Args:
      name: A unique name for the repository.
      register: Whether to register the toolchains (must be set to False if bzlmod is enabled)
    """
    _config_python3_toolchain(name = name)
    if register:
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
