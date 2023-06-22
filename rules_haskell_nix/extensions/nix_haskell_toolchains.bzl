load(
    "@rules_haskell_nix//:nixpkgs.bzl",
    "haskell_register_ghc_nixpkgs",
)
load(
    "@rules_nixpkgs_posix//:posix.bzl",
    "nixpkgs_sh_posix_configure",
)
load(
    "@rules_nixpkgs_core//:nixpkgs.bzl",
    "nixpkgs_package",
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

# def _all_toolchains_impl(rctx):
#     content = "\n".join(rctx.attr.toolchains)
#     print("content=", content)
#     rctx.file("BUILD.bazel", content = content)

# _all_toolchains = repository_rule(
#     implementation = _all_toolchains_impl,
#     attrs = {
#         "toolchains": attr.string_list(),
#     },
# )

def _hub_impl(rctx):
    rctx.file("BUILD")

    toolchain_items = ["{}:{}".format(k, json.decode(v)) for k, v in rctx.attr.toolchains.items()]

    # print(toolchain_items)
    ghc_labels_items = ["{}:{}".format(k, v) for k, v in rctx.attr.ghc_labels.items()]
    # The Label constructors need to be resolved in the hub repository which has visibility

    rctx.file(
        "nix_ghcs.bzl",
        # content = "nix_ghcs = {{ {} }} ".format(",".join(items)),
        content = """
ghc_labels = {{ {ghc_labels} }}
toolchain_configs = {{ {toolchain_configs} }}
toolchain_keys = {toolchain_keys}
toolchains_2 = {toolchains_2}
""".format(
            # toolchain_configs = {k: json.decode(v) for k, v in rctx.attr.toolchains.items()},
            toolchain_configs = ",".join(toolchain_items),
            ghc_labels = ",".join(ghc_labels_items),
            ghc_labels_2 = rctx.attr.ghc_labels_2,
            toolchain_keys = rctx.attr.toolchain_keys,
            toolchains_2 = rctx.attr.toolchains_2,
        ),
    )
    # repository_ctx.file(
    #     "ghc_paths.bzl",
    # )

hub = repository_rule(
    implementation = _hub_impl,
    attrs = {
        "ghc_labels": attr.string_dict(),
        "ghc_labels_2": attr.string(),
        "toolchains": attr.string_dict(),
        "toolchains_2": attr.string(),
        "toolchain_keys": attr.string(),
    },
)

def _nix_haskell_toolchains_impl(mctx):
    # Instead of creating one external repository for each `toolchain(...)` declaration,
    # we recover them in a list and declare them all in the `all_bindist_toolchains` repository.
    # In a way that respects
    # bazel's iteration order over modules.
    # toolchains declared by the root have priority

    # an alternative would be to use aliases to the (to get rid of the toolchain_declarations parameter)
    # if/once the following issue is resolved
    # https://github.com/bazelbuild/bazel/issues/16298
    toolchain_declarations = []
    # for module_index, module in enumerate(mctx.modules):
    #     for config_tag_index, config_tag in enumerate(module.tags.config):
    #         name = "nix_toolchain_{}_{}_{}".format(module_index, config_tag_index, config_tag.name)
    #         all_names.append(name)

    ghc_labels = {}
    toolchains = {}
    toolchain_keys = []  # to remember the order
    for module_index, module in enumerate(mctx.modules):
        for config_tag_index, config_tag in enumerate(module.tags.config):
            if (module.name, module.version, config_tag.name) in toolchains:
                fail("Same module is trying to define nix toolchains with the same name")

            #name = "nix_toolchain_{}_{}_{}".format(module_index, config_tag_index, config_tag.name)
            name = "nix_toolchain_{}_{}_{}".format(module.name, module.version, config_tag.name)
            nixpkgs_ghc_repo_name = "{}_ghc_nixpkgs".format(name)
            nixpkgs_sh_posix_repo_name = "{}_sh_posix_nixpkgs".format(name)
            haskell_toolchain_repo_name = "{}_ghc_nixpkgs_haskell_toolchain".format(name)
            toolchain_repo_name = "{}_ghc_nixpkgs_toolchain".format(name)
            toolchain_declarations.append("# module:{} tag:config_{}".format(module.name, config_tag_index))

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
            if config_tag.sh_posix_attributes != None:
                sh_posix_nixpkgs_kwargs["packages"] = config_tag.sh_posix_attributes

            # TODO: register posix toolchain.
            nixpkgs_sh_posix_configure(
                name = nixpkgs_sh_posix_repo_name,
                register = False,
                **sh_posix_nixpkgs_kwargs
            )

            # toolchains[(module.name, module.version, config_tag.name)] = struct(
            #     version = config_tag.version,
            #     name = name,
            #     static_runtime = config_tag.static_runtime,
            #     fully_static_link = config_tag.fully_static_link,
            #     # build_file = config_tag.build_file,
            #     # build_file_content = config_tag.build_file_content,
            #     ghcopts = config_tag.ghcopts,
            #     compiler_flags_select = config_tag.compiler_flags_select,
            #     haddock_flags = config_tag.haddock_flags,
            #     repl_ghci_args = config_tag.repl_ghci_args,
            #     cabalopts = config_tag.cabalopts,
            #     locale_archive = config_tag.locale_archive,
            #     # attribute_path = config_tag.attribute_path,
            #     sh_posix_attributes = config_tag.sh_posix_attributes,
            #     # nix_file = config_tag.nix_file,
            #     # nix_file_deps = config_tag.nix_file_deps,
            #     # nixopts = config_tag.nixopts,
            #     locale = config_tag.locale,
            #     # repositories = config_tag.repositories,
            #     # repository = config_tag.repository,
            #     # nix_file_content = config_tag.nix_file_content,
            #     exec_constraints = config_tag.exec_constraints,
            #     target_constraints = config_tag.target_constraints,
            # )

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
                # build_file = config_tag.build_file,
                # build_file_content = config_tag.build_file_content,
                "ghcopts": config_tag.ghcopts,
                "compiler_flags_select": config_tag.compiler_flags_select,
                "haddock_flags": config_tag.haddock_flags,
                "repl_ghci_args": config_tag.repl_ghci_args,
                "cabalopts": config_tag.cabalopts,
                "locale_archive": config_tag.locale_archive,
                # attribute_path : config_tag.attribute_path,
                # "sh_posix_attributes": config_tag.sh_posix_attributes,
                # nix_file : config_tag.nix_file,
                # nix_file_deps : config_tag.nix_file_deps,
                # nixopts : config_tag.nixopts,
                "locale": config_tag.locale,
                # repositories : config_tag.repositories,
                # repository : config_tag.repository,
                # nix_file_content : config_tag.nix_file_content,
                "exec_constraints": config_tag.exec_constraints,
                "target_constraints": config_tag.target_constraints,
            }
            ghc_labels[(module.name, module.version, config_tag.name)] = """str(Label("@{}//:bin/ghc"))""".format(nixpkgs_ghc_repo_name)
            toolchain_keys.append((module.name, module.version, config_tag.name))
    hub(
        name = "hub",
        # ghc_labels = {repr(k): v for k, v in ghc_labels.items()},
        ghc_labels = {repr(k): v for k, v in ghc_labels.items()},
        ghc_labels_2 = repr(ghc_labels),
        toolchains = {repr(k): json.encode(v) for k, v in toolchains.items()},
        toolchains_2 = repr(toolchains),
        toolchain_keys = repr(toolchain_keys),
    )
    # _all_toolchains(
    #     name = "all_nix_toolchains",
    #     toolchains = toolchain_declarations,
    # )

nix_haskell_toolchains = module_extension(
    implementation = _nix_haskell_toolchains_impl,
    tag_classes = {
        "config": _config_tag,
    },
)
