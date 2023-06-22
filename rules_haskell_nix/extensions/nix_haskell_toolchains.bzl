load(
    "@rules_nixpkgs_posix//:posix.bzl",
    "nixpkgs_sh_posix_configure",
)
load(
    "@rules_nixpkgs_core//:util.bzl",
    "default_constraints",
    "ensure_constraints_pure",
)
load(
    "@rules_nixpkgs_core//:nixpkgs.bzl",
    "nixpkgs_package",
)

# Based on _nixpkgs_sh_posix_toolchain from
# https://github.com/tweag/rules_nixpkgs/blob/420370f64f03ed9c1ff9b5e2994d06c0439cb1f2/toolchains/posix/posix.bzl#LL109C1-L128C1
# Until the api is modified to register posix toolchains another way (maybe direcly via a module extension from rules_nixpkgs)
# or we can register the toolchains via an alias (https://github.com/bazelbuild/bazel/issues/16298)
def _nixpkgs_sh_posix_toolchain_str(mctx, name, workspace, exec_constraints = []):
    exec_constraints, _ = ensure_constraints_pure(
        default_constraints = default_constraints(mctx),
        exec_constraints = exec_constraints,
    )
    return """
toolchain(
    name = "{name}",
    toolchain = "@{workspace}//:nixpkgs_sh_posix",
    toolchain_type = "@rules_sh//sh/posix:toolchain_type",
    exec_compatible_with = {exec_constraints},
    target_compatible_with = [],
)
    """.format(
        name = name,
        workspace = workspace,
        exec_constraints = exec_constraints,
    )

def _all_posix_toolchains_impl(rctx):
    rctx.file("BUILD.bazel", content = "\n".join(rctx.attr.toolchains))

_all_posix_toolchains = repository_rule(
    implementation = _all_posix_toolchains_impl,
    attrs = {
        "toolchains": attr.string_list(),
    },
    doc = "repository containing `toolchain` declarations for associated posix tooolchains that need to be registered",
)
_config_tag = tag_class(
    attrs = {
        "version": attr.string(
            mandatory = True,
        ),
        "name": attr.string(
            # mandatory = True,
            default = "rules_haskell",
        ),
        "static_runtime": attr.bool(),
        "fully_static_link": attr.bool(),
        "build_file": attr.label(),
        "build_file_content": attr.string(),
        "ghcopts": attr.string_list(),
        "compiler_flags_select": attr.string_list_dict(),
        "haddock_flags": attr.string_list(),
        "repl_ghci_args": attr.string_list(),
        "cabalopts": attr.string_list(),  # OK None = []
        "locale_archive": attr.string(),  # OK
        "attribute_path": attr.string(
            default = "haskellPackages.ghc",
        ),
        "sh_posix_attributes": attr.string_list(),  # [TODO] is None != [] ?
        "nix_file": attr.label(),
        "nix_file_deps": attr.label_list(),
        "nixopts": attr.string_list(),
        "locale": attr.string(
            default = "C.UTF-8",
        ),
        "repositories": attr.string_dict(),
        "repository": attr.label(),
        "nix_file_content": attr.string(),
        "exec_constraints": attr.string_list(),  # [] != None
        "target_constraints": attr.string_list(),  # [] != None
    },
    doc = "creates a new toolchain ",
)

def _hub_impl(rctx):
    rctx.file("BUILD")

    # The Label constructors need to be resolved in the hub repository which has visibility
    ghc_labels_items = ["{}:{}".format(k, v) for k, v in rctx.attr.ghc_labels.items()]

    rctx.file(
        "nix_ghcs.bzl",
        content = """
ghc_labels = {{ {ghc_labels} }}
toolchain_keys = {toolchain_keys}
toolchains_2 = {toolchains_2}
""".format(
            ghc_labels = ",".join(ghc_labels_items),
            toolchain_keys = rctx.attr.toolchain_keys,
            toolchains_2 = rctx.attr.toolchains_2,
        ),
    )

hub = repository_rule(
    implementation = _hub_impl,
    attrs = {
        "ghc_labels": attr.string_dict(),
        "toolchains_2": attr.string(),
        "toolchain_keys": attr.string(),
    },
)

def _nix_haskell_toolchains_impl(mctx):
    ghc_labels = {}
    toolchains = {}
    toolchain_keys = []  # to remember the order
    posix_toolchains = []
    for module in mctx.modules:
        for config_tag in module.tags.config:
            if (module.name, module.version, config_tag.name) in toolchains:
                fail(
                    """module "{module}~{version}" used the "config" tag twice with the "{tag_name}" name""".format(
                        tag_name = config_tag.name,
                        module = module.name,
                        version = module.version,
                    ),
                )
            name = "nix_toolchain_{}_{}_{}".format(module.name, module.version, config_tag.name)
            nixpkgs_ghc_repo_name = "{}_ghc_nixpkgs".format(name)
            nixpkgs_sh_posix_repo_name = "{}_sh_posix_nixpkgs".format(name)
            haskell_toolchain_repo_name = "{}_ghc_nixpkgs_haskell_toolchain".format(name)
            toolchain_repo_name = "{}_ghc_nixpkgs_toolchain".format(name)

            nixpkgs_package(
                name = nixpkgs_ghc_repo_name,
                attribute_path = config_tag.attribute_path,
                build_file = config_tag.build_file,
                build_file_content = config_tag.build_file_content,
                nix_file = config_tag.nix_file,
                nix_file_deps = config_tag.nix_file_deps,
                nix_file_content = config_tag.nix_file_content,
                nixopts = config_tag.nixopts,
                repositories = config_tag.repositories,
                repository = config_tag.repository,
            )

            # Unix tools toolchain required for Cabal packages
            sh_posix_nixpkgs_kwargs = dict(
                nix_file_deps = config_tag.nix_file_deps,
                nixopts = config_tag.nixopts,
                repositories = config_tag.repositories,
                repository = config_tag.repository,
            )
            if config_tag.sh_posix_attributes != []:
                sh_posix_nixpkgs_kwargs["packages"] = config_tag.sh_posix_attributes

            nixpkgs_sh_posix_configure(
                name = nixpkgs_sh_posix_repo_name,
                register = False,
                **sh_posix_nixpkgs_kwargs
            )

            posix_toolchains.append(
                _nixpkgs_sh_posix_toolchain_str(
                    name = "{}_{}_{}_posix_toolchain".format(
                        module.name,
                        module.version,
                        config_tag.name,
                    ),
                    mctx = mctx,
                    workspace = nixpkgs_sh_posix_repo_name,
                    exec_constraints = [],
                ),
            )

            # For convenience we only use one tag and propagate the
            # toolchain configuration to the `declare_toolchains`
            # extension.
            # A tag could be used in the declare_toolchain
            # extension instead, and th
            #
            toolchains[(module.name, module.version, config_tag.name)] = {
                "version": config_tag.version,
                "name": name,
                "static_runtime": config_tag.static_runtime,
                "fully_static_link": config_tag.fully_static_link,
                "ghcopts": config_tag.ghcopts,
                "compiler_flags_select": config_tag.compiler_flags_select,
                "haddock_flags": config_tag.haddock_flags,
                "repl_ghci_args": config_tag.repl_ghci_args,
                "cabalopts": config_tag.cabalopts,
                "locale_archive": config_tag.locale_archive,
                "locale": config_tag.locale,
                "exec_constraints": config_tag.exec_constraints,
                "target_constraints": config_tag.target_constraints,
            }
            ghc_labels[(module.name, module.version, config_tag.name)] = """str(Label("@{}//:bin/ghc"))""".format(nixpkgs_ghc_repo_name)
            toolchain_keys.append((module.name, module.version, config_tag.name))
    hub(
        name = "hub",
        ghc_labels = {repr(k): v for k, v in ghc_labels.items()},
        toolchains_2 = repr(toolchains),
        toolchain_keys = repr(toolchain_keys),
    )
    _all_posix_toolchains(
        name = "all_posix_toolchains",
        toolchains = posix_toolchains,
    )

nix_haskell_toolchains = module_extension(
    implementation = _nix_haskell_toolchains_impl,
    tag_classes = {
        "config": _config_tag,
    },
)
