load("@bazel_skylib//lib:paths.bzl", "paths")
load(
    "//haskell:private/workspace_utils.bzl",
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
load("//haskell/asterius:asterius_config.bzl", "asterius_cabalopts")
load(
    "//haskell/asterius:asterius_dependencies.bzl",
    _asterius_dependencies_bindist = "asterius_dependencies_bindist",
    _asterius_dependencies_custom = "asterius_dependencies_custom",
    _asterius_dependencies_nix = "asterius_dependencies_nix",
)
load("@rules_cc//cc:find_cc_toolchain.bzl", "CC_TOOLCHAIN_TYPE")

# For now the asterius bundle is built and uploaded manually for linux.
# When the asterius build works inside the ci we will be able to download the artifacts from there.
AHC_BINDIST = \
    {
        "0.0.1": {
            "linux_amd64": (
                "https://github.com/tweag/asterius_bundles_for_bazel/releases/download/0.0.2/asterius_bundle.tar.gz",
                "bb051b73547e5b35e77ececbbca59a551cb45bca34c87ad1f09e166ccf1210e6",
            ),
        },
    }

AHC_DEFAULT_VERSION = "0.0.1"

def _labels_from_bundle_name(bundle_repo_name, asterius_version):
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
    exec_platform = repository_ctx.attr.exec_platform
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

_asterius_bundle = repository_rule(
    _asterius_bundle_impl,
    local = False,
    attrs = {
        "version": attr.string(),
        "exec_platform": attr.string(),
    },
    doc = "Downloads and extracts an asterius bundle.",
)

def _ahc_toolchain_impl(ctx):
    exec_platform = platform_of_constraints(ctx.attr.exec_constraints)
    exec_constraints_str = [str(c) for c in ctx.attr.exec_constraints]
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
    toolchain_type = "@rules_haskell//haskell/asterius:toolchain_type",
    toolchain = "@{bindist_name}//:asterius-toolchain-impl",
    exec_compatible_with = {exec_constraints},
)

toolchain(
    name = "wasm_cc_toolchain",
    exec_compatible_with = {exec_constraints},
    target_compatible_with = {target_constraints},
    toolchain = "{wasm_cc_toolchain}",
    toolchain_type = "{cc_toolchain}",
)
        """.format(
            bindist_name = ctx.attr.bindist_name,
            exec_constraints = exec_constraints_str,
            target_constraints = target_constraints,
            exec_platform = exec_platform,
            wasm_cc_toolchain = ctx.attr.wasm_cc_toolchain,
            cc_toolchain = CC_TOOLCHAIN_TYPE,
        ),
    )

_ahc_toolchain = repository_rule(
    _ahc_toolchain_impl,
    local = False,
    attrs = {
        "bindist_name": attr.string(),
        "exec_constraints": attr.label_list(),
        "wasm_cc_toolchain": attr.label(
            mandatory = True,
            doc = "A web assembly cc_toolchain",
        ),
    },
    doc = "Declares WebAssembly related toolchains from the bindist_name repository.",
)

def _ahc_impl(ctx):
    filepaths = resolve_labels(ctx, [
        "@rules_haskell//haskell:ahc.BUILD.tpl",
        "@rules_haskell//haskell:private/pkgdb_to_bzl.py",
    ])
    lib_path = str(ctx.path(ctx.attr.asterius_lib_setting_file).dirname)
    ahc_pkg_path = ctx.path(ctx.attr.ahc_pkg)

    docdir_path = execute_or_fail_loudly(ctx, [ahc_pkg_path, "field", "base", "haddock-html", "--simple-output"]).stdout.strip()
    ctx.symlink(lib_path, "asterius_lib")

    toolchain_libraries = pkgdb_to_bzl(ctx, filepaths, paths.basename(lib_path))
    exec_os = os_of_constraints(ctx.attr.exec_constraints)
    locale = ctx.attr.locale or ("en_US.UTF-8" if exec_os == "darwin" else "C.UTF-8")

    toolchain = define_rule(
        "haskell_toolchain",
        name = "toolchain-impl",
        asterius_binaries = repr(str(ctx.attr.asterius_binaries)),
        tools = [str(ctx.attr.full_bundle)],
        libraries = "toolchain_libraries",
        libdir_path = repr(paths.basename(lib_path)),
        docdir_path = repr(docdir_path),
        version = repr(ctx.attr.version),
        static_runtime = True,
        fully_static_link = True,
        ghcopts = ctx.attr.ghcopts,
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
        filepaths["@rules_haskell//haskell:ahc.BUILD.tpl"],
        substitutions = {
            "%{toolchain_libraries}": toolchain_libraries["file_content"],
            "%{toolchain}": toolchain,
            "%{asterius_toolchain}": asterius_toolchain,
        },
        executable = False,
    )

    # We make the list of toolchain libraries available for loading in the WORKSPACE file.
    ctx.file(
        "toolchain_libraries.bzl",
        executable = False,
        content = "toolchain_libraries = {}".format(toolchain_libraries["toolchain_libraries"]),
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
        "repl_ghci_args": attr.string_list(),
        "ghcopts": attr.string_list(),
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

def rules_haskell_asterius_toolchain(
        name,
        version,
        exec_constraints,
        asterius_lib_setting_file,
        ahc_pkg,
        asterius_binaries,
        full_bundle,
        wasm_cc_toolchain,
        ghcopts = None,
        repl_ghci_args = None,
        cabalopts = None,
        locale = None,
        register = True):
    """ Define and (optionally) registers asterius related toolchains.

    Args:
      name: A unique name for the repository.
      version: The asterius version for this toolchain.
      exec_constraints: Executions constraints for the toolchain resolution.
      asterius_lib_setting_file: Label of the `settings` file in the `asterius_lib` repository of the asterius installation.
      ahc_pkg: Label of the `ahc-pkg` binary.
      asterius_binaries: Filegroup containing the asterius top-level binaries.
      full_bundle: Filegroup with the asterius_binaries and all their dependencies.
      wasm_cc_toolchain: A web asssembly cc toolchain.
      ghcopts: [see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-ghcopts)
      haddock_flags: [see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-haddock_flags)
      repl_ghci_args: [see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-repl_ghci_args)
      cabalopts: [see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-cabalopts)
      locale: [see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-locale)
      register: Whether to register the toolchains (must be set to False if bzlmod is activated)
    """

    _ahc(
        name = name,
        version = version,
        exec_constraints = exec_constraints,
        ghcopts = ghcopts,
        repl_ghci_args = repl_ghci_args,
        cabalopts = cabalopts,
        locale = locale,
        asterius_lib_setting_file = asterius_lib_setting_file,
        ahc_pkg = ahc_pkg,
        asterius_binaries = asterius_binaries,
        full_bundle = full_bundle,
    )

    toolchain_name = "{}-toolchain".format(name)

    _ahc_toolchain(
        name = toolchain_name,
        bindist_name = name,
        exec_constraints = exec_constraints,
        wasm_cc_toolchain = wasm_cc_toolchain,
    )
    if register:
        native.register_toolchains("@{}//:toolchain".format(toolchain_name))
        native.register_toolchains("@{}//:asterius_toolchain".format(toolchain_name))
        native.register_toolchains("@{}//:wasm_cc_toolchain".format(toolchain_name))

def rules_haskell_asterius_toolchains(
        version = AHC_DEFAULT_VERSION,
        ghcopts = [],
        cabalopts = [],
        repl_ghci_args = [],  # @unused
        locale = None,
        register = True):
    """
    Create and (optionally) register Asterius related toolchains for all platforms.

    Args:
      version: Asterius version.
      compiler_flags: [see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-compiler_flags)
      ghcopts: [see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-ghcopts)
      cabalopts: [see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-cabalopts)
      repl_ghci_args: [see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-repl_ghci_args)
      locale: [see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-locale)
      register: Whether to register the toolchains (must be set to False if bzlmod is activated)

    """
    if not AHC_BINDIST.get(version):
        fail("Binary distribution of Asterius {} not available.".format(version))
    for platform in AHC_BINDIST[version]:
        # Download the asterius bundle.
        bundle_repo_name = "asterius_bundle_{}".format(platform)
        _asterius_bundle(
            name = bundle_repo_name,
            version = version,
            exec_platform = platform,
        )

        os, _, arch = platform.partition("_")
        os_constraint = {
            "darwin": "@platforms//os:osx",
            "linux": "@platforms//os:linux",
            "windows": "@platforms//os:windows",
        }.get(os)
        arch_constraint = {
            "amd64": "@platforms//cpu:x86_64",
        }.get(arch)
        exec_constraints = [os_constraint, arch_constraint]

        # Create and register the asterius toolchains
        (asterius_lib_setting_file, ahc_pkg, asterius_binaries, full_bundle, wasm_cc_toolchain) = _labels_from_bundle_name(bundle_repo_name, version)

        rules_haskell_asterius_toolchain(
            "{}_asterius".format(platform),
            version,
            exec_constraints,
            asterius_lib_setting_file,
            ahc_pkg,
            asterius_binaries,
            full_bundle,
            wasm_cc_toolchain,
            ghcopts = ghcopts,
            cabalopts = cabalopts,
            locale = locale,
            register = register,
        )

def asterius_dependencies_bindist(**kwargs):
    """ Install asterius dependencies for bindists.

    Args:
      package_json: The package.json file that will be passed to [yarn_install](https://bazelbuild.github.io/rules_nodejs/Built-ins.html#yarn_install)
      yarn_lock: The yarn.lock file that will be passed to [yarn_install](https://bazelbuild.github.io/rules_nodejs/Built-ins.html#yarn_install)

    """
    _asterius_dependencies_bindist(**kwargs)

def asterius_dependencies_nix(**kwargs):
    """ Install asterius dependencies based on nix.

    Args:
      nix_repository: The nix repository from which we try to install node.
      nixpkgs_package_rule: The `nixpkgs_package` rule from `rules_nixpkgs`.
      package_json: The package.json file that will be passed to [yarn_install](https://bazelbuild.github.io/rules_nodejs/Built-ins.html#yarn_install)
      yarn_lock: The yarn.lock file that will be passed to [yarn_install](https://bazelbuild.github.io/rules_nodejs/Built-ins.html#yarn_install)
      nixpkgs_nodejs: The prefix for the repository containing node that will be installed with `nixpkgs_package`.
    """
    _asterius_dependencies_nix(**kwargs)

def asterius_dependencies_custom(**kwargs):
    """ Setup asterius dependencies using an existing [rules_nodejs](https://github.com/bazelbuild/rules_nodejs) installation with webpack.

    Args:
      webpack_rule: The label of a webpack rule declared with rules_nodejs.
        For instance as such:
        ```
        load("@npm//webpack-cli:index.bzl", webpack = "webpack_cli")
        webpack(
            name = "webpack",
            visibility = ["//visibility:public"],
        )
        ```
    """
    _asterius_dependencies_custom(**kwargs)

def _toolchain_libraries_impl(repository_ctx):
    if repository_ctx.attr.repository:
        repository_ctx.file(
            "toolchain_libraries.bzl",
            executable = False,
            content = """\
load("@{}//:toolchain_libraries.bzl", _toolchain_libraries = "toolchain_libraries")
toolchain_libraries = _toolchain_libraries
    """.format(repository_ctx.attr.repository),
        )
    else:
        repository_ctx.file(
            "toolchain_libraries.bzl",
            executable = False,
            content = "toolchain_libraries = None",
        )

    repository_ctx.file("BUILD", executable = False, content = "")

toolchain_libraries = repository_rule(
    _toolchain_libraries_impl,
    attrs = {
        "repository": attr.string(
            default = "",
            doc = "An optional repository from which we recover the toolchain_libraries variable. If absent or empty, the toolchain_libraries variable will be equal to None.",
        ),
    },
    doc = "Optionally recovers the `toolchain_libraries` variable from another repository",
)
