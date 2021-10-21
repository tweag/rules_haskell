load("@bazel_skylib//lib:paths.bzl", "paths")
load(
    "//haskell:private/workspace_utils.bzl",
    "default_constraints",
    "define_rule",
    "execute_or_fail_loudly",
    "find_python",
    "resolve_labels",
)
load("//haskell:private/validate_attrs.bzl", "check_deprecated_attribute_usage")
load(
    "//haskell:private/pkgdb_to_bzl.bzl",
    "pkgdb_to_bzl",
)
load(
    "//haskell/platforms:list.bzl",
    "os_of_constraints",
    "platform_of_constraints",
)
load("//haskell:private/versions.bzl", "supports_rules_nodejs_ge_4")
load("//haskell/asterius:asterius_config.bzl", "asterius_cabalopts")

# For now the asterius bundle is built and uploaded manually for linux.
# When the asterius build works inside the ci we will be able to download the artifacts from there.
AHC_BINDIST = \
    {
        "0.0.1": {
            "linux_x86_64": (
                "https://github.com/ylecornec/test_bundle/releases/download/test/asterius_bundle.tar.gz",
                "0c50415278e14003541697c99818e96d52ba67b1a140ac0eaf89bfb6b751548f",
            ),
        },
    }

AHC_DEFAULT_VERSION = "0.0.1"

def default_exec_constraints(repository_ctx):
    (_, exec_constraints) = default_constraints(repository_ctx)
    return [Label(c) for c in exec_constraints]

def labels_from_bundle_name(bundle_repo_name, asterius_version):
    """ Labels of targets from the external repository extracted from the asterius bundle."""
    return (
        "@{}//:asterius-{}_data/.boot/asterius_lib/settings".format(
            bundle_repo_name,
            asterius_version,
        ),
        "@{}//:bazel_build/wrappers/ahc-pkg".format(bundle_repo_name),
        "@{}//:asterius_binaries".format(bundle_repo_name),
        "@{}//:local_asterius".format(bundle_repo_name),
        "@{}//:wasm_cc".format(bundle_repo_name),
    )

def _asterius_bundle_impl(repository_ctx):
    # As in the _ghc_nixpkgs_toolchain rule.
    # we use the default constraints if none are provided.
    # We only need exec_contraints as we know we target wasm32
    exec_constraints = repository_ctx.attr.exec_constraints or default_exec_constraints(repository_ctx)
    exec_platform = platform_of_constraints(exec_constraints)

    version = repository_ctx.attr.version
    if version not in AHC_BINDIST or AHC_BINDIST[version].get(exec_platform) == None:
        fail("Operating system {0} does not have a bindist for Asterius version {1}".format(exec_platform, version))
    else:
        url, sha256 = AHC_BINDIST[version][exec_platform]

    repository_ctx.download_and_extract(
        url = url,
        output = "",
        sha256 = sha256,
    )

asterius_bundle = repository_rule(
    _asterius_bundle_impl,
    local = False,
    attrs = {
        "version": attr.string(),
        "exec_constraints": attr.label_list(),
    },
    doc = "Downloads and extracts an asterius bundle.",
)

def _ahc_toolchain_impl(ctx):
    exec_constraints = ctx.attr.exec_constraints or default_exec_constraints(ctx)
    if ctx.attr.nix:
        exec_constraints.append(Label("@io_tweag_rules_nixpkgs//nixpkgs/constraints:support_nix"))
    exec_platform = platform_of_constraints(exec_constraints)

    exec_constraints_str = [str(c) for c in exec_constraints]
    target_constraints = ["@platforms//cpu:wasm32"]

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

toolchain(
    name = "asterius_toolchain",
    toolchain_type = "@rules_haskell//haskell/asterius:asterius-toolchain",
    toolchain = "@{bindist_name}//:asterius-toolchain-impl",
    exec_compatible_with = {exec_constraints},
)

toolchain(
    name = "wasm_cc_toolchain",
    exec_compatible_with = {exec_constraints},
    target_compatible_with = {target_constraints},
    toolchain = "{wasm_cc_toolchain}",
    toolchain_type = "@bazel_tools//tools/cpp:toolchain_type",
)
        """.format(
            bindist_name = ctx.attr.bindist_name,
            exec_constraints = exec_constraints_str,
            target_constraints = target_constraints,
            exec_platform = exec_platform,
            wasm_cc_toolchain = ctx.attr.wasm_cc_toolchain,
        ),
    )

_ahc_toolchain = repository_rule(
    _ahc_toolchain_impl,
    local = False,
    attrs = {
        "bindist_name": attr.string(),
        "exec_constraints": attr.label_list(),
        "nix": attr.bool(
            mandatory = True,
            doc = "Whether this toolchain depends on nix",
        ),
        "wasm_cc_toolchain": attr.label(
            mandatory = True,
            doc = "A web assembly cc_toolchain",
        ),
    },
    doc = "Declares WebAssembly related toolchains from the bindist_name repository.",
)

def _ahc_impl(ctx):
    filepaths = resolve_labels(ctx, [
        "@rules_haskell//haskell:ghc.BUILD.tpl",
        "@rules_haskell//haskell:private/pkgdb_to_bzl.py",
    ])
    lib_path = str(ctx.path(ctx.attr.asterius_lib_setting_file).dirname)
    ahc_pkg_path = ctx.path(ctx.attr.ahc_pkg)

    docdir_path = execute_or_fail_loudly(ctx, [ahc_pkg_path, "field", "base", "haddock-html", "--simple-output"]).stdout.strip()
    ctx.symlink(lib_path, "asterius_lib")

    toolchain_libraries = pkgdb_to_bzl(ctx, filepaths, paths.basename(lib_path))

    exec_constraints = ctx.attr.exec_constraints or default_exec_constraints(ctx)
    exec_os = os_of_constraints(exec_constraints)
    locale = ctx.attr.locale or ("en_US.UTF-8" if exec_os == "darwin" else "C.UTF-8")

    non_asterius_binaries = "@{}//:bin".format(ctx.attr.ghc_repo_name)
    toolchain = define_rule(
        "haskell_toolchain",
        name = "toolchain-impl",
        asterius_binaries = repr(str(ctx.attr.asterius_binaries)),
        tools =
            [
                str(ctx.attr.asterius_binaries),
                str(ctx.attr.full_bundle),
                non_asterius_binaries,
            ],
        libraries = "toolchain_libraries",
        libdir_path = repr(paths.basename(lib_path)),
        docdir_path = repr(docdir_path),
        version = repr(ctx.attr.version),
        static_runtime = True,
        fully_static_link = True,
        ghcopts = ctx.attr.ghcopts,
        haddock_flags = ctx.attr.haddock_flags,
        repl_ghci_args = ctx.attr.repl_ghci_args,
        cabalopts = asterius_cabalopts + ctx.attr.cabalopts,
        locale = repr(locale),
    )

    asterius_toolchain = define_rule(
        "asterius_toolchain",
        name = "asterius-toolchain-impl",
        binaries = [str(ctx.attr.asterius_binaries)],
        tools = [str(ctx.attr.full_bundle)],
    )

    ctx.template(
        "BUILD",
        filepaths["@rules_haskell//haskell:ghc.BUILD.tpl"],
        substitutions = {
            "%{toolchain_libraries}": toolchain_libraries,
            "%{toolchain}": toolchain,
            "%{asterius_toolchain}": asterius_toolchain,
        },
        executable = False,
    )

_ahc = repository_rule(
    _ahc_impl,
    local = False,
    attrs = {
        "version": attr.string(
            default = AHC_DEFAULT_VERSION,
            values = AHC_BINDIST.keys(),
            doc = "The desired Asterius version",
        ),
        "exec_constraints": attr.label_list(),
        "ghc_repo_name": attr.string(),
        "ghcopts": attr.string_list(),
        "haddock_flags": attr.string_list(),
        "repl_ghci_args": attr.string_list(),
        "cabalopts": attr.string_list(),
        "locale": attr.string(
            mandatory = False,
        ),
        "asterius_lib_setting_file": attr.label(),
        "ahc_pkg": attr.label(doc = "Label for the ahc_pkg binary"),
        "asterius_binaries": attr.label(
            doc = "Filegroup with the asterius binaries.",
        ),
        "full_bundle": attr.label(
            doc = "Filegroup with the full bundle, which is necessary for the binaries to run.",
        ),
    },
    doc = "Defines the haskell toolchain using asterius, as well as the asterius toolchain which contains asterius specific tools.",
)

def ahc(
        name,
        version,
        exec_constraints,
        ghc_repo_name,
        asterius_lib_setting_file,
        ahc_pkg,
        asterius_binaries,
        full_bundle,
        wasm_cc_toolchain,
        compiler_flags = None,
        ghcopts = None,
        haddock_flags = None,
        repl_ghci_args = None,
        cabalopts = None,
        locale = None,
        nix = False):
    """ Define and registers asterius related toolchains. """

    if not supports_rules_nodejs_ge_4(native.bazel_version):
        fail("Asterius is only supported starting from bazel 4 (because it depends on rules_nodejs >= 4)")

    ghcopts = check_deprecated_attribute_usage(
        old_attr_name = "compiler_flags",
        old_attr_value = compiler_flags,
        new_attr_name = "ghcopts",
        new_attr_value = ghcopts,
    )

    # bindist_name = name
    toolchain_name = "{}-toolchain".format(name)

    _ahc(
        name = name,
        version = version,
        exec_constraints = exec_constraints,
        ghc_repo_name = ghc_repo_name,
        ghcopts = ghcopts,
        haddock_flags = haddock_flags,
        repl_ghci_args = repl_ghci_args,
        cabalopts = cabalopts,
        locale = locale,
        asterius_lib_setting_file = asterius_lib_setting_file,
        ahc_pkg = ahc_pkg,
        asterius_binaries = asterius_binaries,
        full_bundle = full_bundle,
    )

    _ahc_toolchain(
        name = toolchain_name,
        bindist_name = name,
        exec_constraints = exec_constraints,
        nix = nix,
        wasm_cc_toolchain = wasm_cc_toolchain,
    )
    native.register_toolchains("@{}//:toolchain".format(toolchain_name))
    native.register_toolchains("@{}//:asterius_toolchain".format(toolchain_name))
    native.register_toolchains("@{}//:wasm_cc_toolchain".format(toolchain_name))

def _asterius_toolchain_impl(ctx):
    ahc_dist = None
    for file in ctx.files.binaries:
        basename_no_ext = paths.split_extension(file.basename)[0]
        if basename_no_ext == "ahc-dist":
            ahc_dist = file
    if ahc_dist == None:
        fail("ahc-dist was not found when defining the asterius toolchain")

    return [
        platform_common.ToolchainInfo(
            name = ctx.label.name,
            ahc_dist = ahc_dist,
            tools = ctx.files.tools,
        ),
    ]

asterius_toolchain = rule(
    _asterius_toolchain_impl,
    attrs = {
        "binaries": attr.label_list(
            mandatory = True,
            doc = "The asterius top level wrappers",
        ),
        "tools": attr.label_list(
            mandatory = True,
            doc = "The complete asterius bundle, which is needed to execute the wrappers.",
        ),
    },
    doc = "Toolchain for asterius tools that are not part of the regular haskell toolchain",
)
