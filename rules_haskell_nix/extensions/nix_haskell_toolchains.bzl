""" The `new` tag from this module extension follows the interface of the
`haskell_register_ghc_nixpkgs` repository rule to configure nix-based
haskell toolchains, as well as a companion posix toolchains.

Once posix toolchains can be registered directly via the rules_nixpkgs_posix module,
the handling of these toolchains could be removed from this extension.
"""

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
load("@rules_haskell//extensions:haskell_toolchains.bzl", "all_toolchains")

# Based on _nixpkgs_sh_posix_toolchain from:
# https://github.com/tweag/rules_nixpkgs/blob/420370f64f03ed9c1ff9b5e2994d06c0439cb1f2/toolchains/posix/posix.bzl#LL109C1-L128C1
# This is needed until rules_nixpkgs_posix can declare toolchains,
# or we can register the toolchains via an alias (https://github.com/bazelbuild/bazel/issues/16298)
def _nixpkgs_sh_posix_toolchain_declaration(mctx, workspace, exec_constraints = []):
    exec_constraints, _ = ensure_constraints_pure(
        default_constraints = default_constraints(mctx),
        exec_constraints = exec_constraints,
    )
    return """
toolchain(
    name = "{{}}",
    toolchain = "@{workspace}//:nixpkgs_sh_posix",
    toolchain_type = "@rules_sh//sh/posix:toolchain_type",
    exec_compatible_with = {exec_constraints},
    target_compatible_with = [],
)
    """.format(
        workspace = workspace,
        exec_constraints = exec_constraints,
    )

_new_tag = tag_class(
    attrs = {
        "version": attr.string(
            mandatory = True,
            doc = "The version of ghc to install",
        ),
        "static_runtime": attr.bool(
            doc = "See [haskell_register_ghc_nixpkgs](nixpkgs.html#haskell_register_ghc_nixpkgs-static_runtime)",
        ),
        "fully_static_link": attr.bool(
            doc = "See [haskell_register_ghc_nixpkgs](nixpkgs.html#haskell_register_ghc_nixpkgs-fully_static_link)",
        ),
        "build_file": attr.label(
            doc = "See [haskell_register_ghc_nixpkgs](nixpkgs.html#haskell_register_ghc_nixpkgs-build_file)",
        ),
        "build_file_content": attr.string(
            doc = "See [haskell_register_ghc_nixpkgs](nixpkgs.html#haskell_register_ghc_nixpkgs-build_file_content)",
        ),
        "ghcopts": attr.string_list(
            doc = "See [haskell_register_ghc_nixpkgs](nixpkgs.html#haskell_register_ghc_nixpkgs-ghcopts)",
        ),
        "compiler_flags_select": attr.string_list_dict(
            doc = "See [haskell_register_ghc_nixpkgs](nixpkgs.html#haskell_register_ghc_nixpkgs-compiler_flags_select)",
        ),
        "haddock_flags": attr.string_list(
            doc = "See [haskell_register_ghc_nixpkgs](nixpkgs.html#haskell_register_ghc_nixpkgs-haddock_flags)",
        ),
        "repl_ghci_args": attr.string_list(
            doc = "See [haskell_register_ghc_nixpkgs](nixpkgs.html#haskell_register_ghc_nixpkgs-repl_ghci_args)",
        ),
        "cabalopts": attr.string_list(
            doc = "See [haskell_register_ghc_nixpkgs](nixpkgs.html#haskell_register_ghc_nixpkgs-cabalopts)",
        ),
        "locale_archive": attr.string(
            doc = "See [haskell_register_ghc_nixpkgs](nixpkgs.html#haskell_register_ghc_nixpkgs-locale_archive)",
        ),
        "attribute_path": attr.string(
            default = "haskellPackages.ghc",
            doc = "See [haskell_register_ghc_nixpkgs](nixpkgs.html#haskell_register_ghc_nixpkgs-attribute_path)",
        ),
        "sh_posix_attributes": attr.string_list(
            doc = "See [haskell_register_ghc_nixpkgs](nixpkgs.html#haskell_register_ghc_nixpkgs-sh_posix_attributes)",
        ),
        "nix_file": attr.label(
            doc = "See [haskell_register_ghc_nixpkgs](nixpkgs.html#haskell_register_ghc_nixpkgs-nix_file)",
        ),
        "nix_file_deps": attr.label_list(
            doc = "See [haskell_register_ghc_nixpkgs](nixpkgs.html#haskell_register_ghc_nixpkgs-nix_file_deps)",
        ),
        "nixopts": attr.string_list(
            doc = "See [haskell_register_ghc_nixpkgs](nixpkgs.html#haskell_register_ghc_nixpkgs-nixopts)",
        ),
        "locale": attr.string(
            default = "C.UTF-8",
            doc = "See [haskell_register_ghc_nixpkgs](nixpkgs.html#haskell_register_ghc_nixpkgs-locale)",
        ),
        "repositories": attr.string_dict(
            doc = "See [haskell_register_ghc_nixpkgs](nixpkgs.html#haskell_register_ghc_nixpkgs-repositories)",
        ),
        "repository": attr.label(
            doc = "See [haskell_register_ghc_nixpkgs](nixpkgs.html#haskell_register_ghc_nixpkgs-repository)",
        ),
        "nix_file_content": attr.string(
            doc = "See [haskell_register_ghc_nixpkgs](nixpkgs.html#haskell_register_ghc_nixpkgs-nix_file_content)",
        ),
        "exec_constraints": attr.string_list(
            doc = "See [haskell_register_ghc_nixpkgs](nixpkgs.html#haskell_register_ghc_nixpkgs-exec_constraints)",
        ),
        "target_constraints": attr.string_list(
            doc = "See [haskell_register_ghc_nixpkgs](nixpkgs.html#haskell_register_ghc_nixpkgs-target_constraints)",
        ),
    },
    doc = "Declare a new nix-based haskell toolchain. See [haskell_register_ghc_nixpkgs](nixpkgs.html#haskell_register_ghc_nixpkgs)",
)

def _toolchains_configuration_impl(rctx):
    rctx.file("BUILD")

    # The labels of ghc binaries need to be resolved in the `nix_haskell_toolchains_configurations` repository
    # which has visibility over the repositories generated via nixpkgs_package
    ghc_labels_items = ["{}:{}".format(k, v) for k, v in rctx.attr.ghc_labels.items()]

    rctx.file(
        "nix_haskell_toolchains_configurations.bzl",
        content = """
ghc_labels = {{ {ghc_labels} }}
toolchain_ids = {toolchain_ids}
toolchains = {toolchains}
""".format(
            ghc_labels = ",".join(ghc_labels_items),
            toolchain_ids = rctx.attr.toolchain_ids,
            toolchains = rctx.attr.toolchains,
        ),
    )

toolchains_configuration = repository_rule(
    implementation = _toolchains_configuration_impl,
    attrs = {
        "toolchain_ids": attr.string(
            doc = "The ids of the toolchains to declare",
        ),
        "ghc_labels": attr.string_dict(
            doc = "The labels of the corresponding ghc binaries to resolve",
        ),
        "toolchains": attr.string(
            doc = "The configuration of these toolchains",
        ),
    },
    doc = """ This repository is used to canonicalize the labels of the ghc binaries.

Since the names of the repositories generated by `nixpkgs_package` depend on which tags are used,
we cannot call `use_repo()` in the `MODULE.bazel` file to make them visible to `rules_haskell_nix`.

They are however visible from this `nix_haskell_toolchains_configurations` repository which is generated by the same module extension.
This `nix_haskell_toolchains_configurations` is then loaded by the `declare_toolchains` extension, which sets up and generates the `all_nix_toolchains` repository.
""",
)

def _nix_haskell_toolchains_impl(mctx):
    """ This extension generates the nix repositories and forwards haskell related configuration to the `declare_toolchain` extension (via the @nix_haskell_toolchains_configurations repository). """
    ghc_labels = {}
    toolchains_configurations = {}
    haskell_toolchain_ids = []  # Used to track the order of the toolchains
    posix_toolchains = []
    for module in mctx.modules:
        for (index, new_tag) in enumerate(module.tags.new):
            name = "{}_{}_{}".format(module.name, module.version, index)
            nixpkgs_ghc_repo_name = "{}_ghc_nixpkgs".format(name)
            nixpkgs_sh_posix_repo_name = "{}_sh_posix_nixpkgs".format(name)

            nixpkgs_package(
                name = nixpkgs_ghc_repo_name,
                attribute_path = new_tag.attribute_path,
                build_file = new_tag.build_file,
                build_file_content = new_tag.build_file_content,
                nix_file = new_tag.nix_file,
                nix_file_deps = new_tag.nix_file_deps,
                nix_file_content = new_tag.nix_file_content,
                nixopts = new_tag.nixopts,
                repositories = new_tag.repositories,
                repository = new_tag.repository,
            )

            # Unix tools toolchain required for Cabal packages
            sh_posix_nixpkgs_kwargs = dict(
                nix_file_deps = new_tag.nix_file_deps,
                nixopts = new_tag.nixopts,
                repositories = new_tag.repositories,
                repository = new_tag.repository,
            )
            if new_tag.sh_posix_attributes != []:
                sh_posix_nixpkgs_kwargs["packages"] = new_tag.sh_posix_attributes
            nixpkgs_sh_posix_configure(
                name = nixpkgs_sh_posix_repo_name,
                register = False,
                **sh_posix_nixpkgs_kwargs
            )

            posix_toolchains.append(
                _nixpkgs_sh_posix_toolchain_declaration(
                    mctx = mctx,
                    workspace = nixpkgs_sh_posix_repo_name,
                ),
            )
            haskell_toolchain_ids.append((module.name, module.version, index))
            toolchains_configurations[(module.name, module.version, index)] = {
                "version": new_tag.version,
                "name": name,
                "static_runtime": new_tag.static_runtime,
                "fully_static_link": new_tag.fully_static_link,
                "ghcopts": new_tag.ghcopts,
                "compiler_flags_select": new_tag.compiler_flags_select,
                "haddock_flags": new_tag.haddock_flags,
                "repl_ghci_args": new_tag.repl_ghci_args,
                "cabalopts": new_tag.cabalopts,
                "locale_archive": new_tag.locale_archive,
                "locale": new_tag.locale,
                "exec_constraints": new_tag.exec_constraints,
                "target_constraints": new_tag.target_constraints,
            }

            # ghc_labels are handled separately to be resolved when loading the nix_haskell_toolchains_configurations.bzl file later
            ghc_labels[(module.name, module.version, index)] = """str(Label("@{}//:bin/ghc"))""".format(nixpkgs_ghc_repo_name)
    toolchains_configuration(
        name = "nix_haskell_toolchains_configurations",
        ghc_labels = {repr(k): v for k, v in ghc_labels.items()},
        toolchains = repr(toolchains_configurations),
        toolchain_ids = repr(haskell_toolchain_ids),
    )

    all_toolchains(
        name = "all_posix_toolchains",
        toolchains = posix_toolchains,
    )

nix_haskell_toolchains = module_extension(
    implementation = _nix_haskell_toolchains_impl,
    tag_classes = {
        "new": _new_tag,
    },
    doc = "This extension is used to install and declare nix-based haskell toolchains.",
)
