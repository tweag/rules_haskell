load(
    "@rules_haskell//haskell:ghc_bindist.bzl",
    "ghc_bindist",
    # "ghc_bindist_toolchain",
    "haskell_register_ghc_bindists",
)

_bindists_tag = tag_class(
    attrs = {
        "version": attr.string(),
        "ghcopts": attr.string_list(),
        "haddock_flags": attr.string_list(),
        "repl_ghci_args": attr.string_list(),
        "cabalopts": attr.string_list(),
        "locale": attr.string(),
    },
    doc = "creates a new toolchain ",
)

_bindist_tag = tag_class(
    attrs = {
        "name": attr.string(
            mandatory = True,
        ),
        "version": attr.string(
            mandatory = True,
        ),
        "target": attr.string(
            mandatory = True,
        ),
        "ghcopts": attr.string_list(),
        "haddock_flags": attr.string_list(),
        "repl_ghci_args": attr.string_list(),
        "cabalopts": attr.string_list(),
        "locale": attr.string(),
    },
    doc = "creates a new toolchain ",
)

def _all_toolchains_impl(rctx):
    #     toolchains = []
    #     for (i, toolchain) in enumerate(rctx.attr.toolchains):
    #         # Registering toolchains with alias does not work yet, so we copy the content of the BUILD files.
    #         # https://github.com/bazelbuild/bazel/issues/16298
    #         build_file = Label("@@{}//:BUILD".format(toolchain.workspace_name))
    #         s = rctx.read(rctx.path(build_file))
    #         print("BUILD=", s)

    #     toolchains = [
    #         """
    # alias(
    #     name = "toolchain_{i}",
    #     actual = {actual},
    # )
    # """.format(i = i, actual = repr(str(toolchain)))
    #         for (i, toolchain) in enumerate(rctx.attr.toolchains)
    #     ]
    # rctx.file("BUILD.bazel", content = "\n".join(toolchains))
    content = "\n".join(rctx.attr.toolchains)
    print("content=", content)
    rctx.file("BUILD.bazel", content = content)

_all_toolchains = repository_rule(
    implementation = _all_toolchains_impl,
    attrs = {
        # "toolchains": attr.label_list(),
        "toolchains": attr.string_list(),
    },
)

def _haskell_toolchains_impl(mctx):
    # We only consider the first `bindists` tag, because subsequent
    # ones would have the same constraints and lower priority.
    found_bindists = False

    # Instead of creating one external repository for each `toolchain(...)` declaration,
    # we recover them in a list and declare them all in the `all_bindist_toolchains` repository.
    # In a way that respects
    # bazel's iteration order over modules.
    # toolchains declared by the root have priority

    # an alternative would be to use aliases to the (to get rid of the toolchain_declarations parameter)
    # if/once the following issue is resolved
    # https://github.com/bazelbuild/bazel/issues/16298
    toolchain_declarations = []

    for module_index, module in enumerate(mctx.modules):
        for bindist_tag_index, bindist_tag in enumerate(module.tags.bindist):
            name = "bindist_{}_{}_{}".format(module_index, bindist_tag_index, bindist_tag.name)
            toolchain_declarations.append("# module:{} tag:bindist_{}".format(module.name, bindist_tag_index))
            ghc_bindist(
                name = name,
                version = bindist_tag.version,
                target = bindist_tag.target,
                ghcopts = bindist_tag.ghcopts,
                haddock_flags = bindist_tag.haddock_flags,
                repl_ghci_args = bindist_tag.repl_ghci_args,
                cabalopts = bindist_tag.cabalopts,
                locale = bindist_tag.locale,
                register = False,
                toolchain_declarations = toolchain_declarations,
            )

        for bindists_tag_index, bindists_tag in enumerate(module.tags.bindists):
            if not found_bindists:
                found_bindists = True
                toolchain_declarations.append("# module:{} tag:bindists_{}".format(module.name, bindists_tag_index))
                haskell_register_ghc_bindists(
                    version = bindists_tag.version,
                    ghcopts = bindists_tag.ghcopts,
                    haddock_flags = bindists_tag.haddock_flags,
                    repl_ghci_args = bindists_tag.repl_ghci_args,
                    cabalopts = bindists_tag.cabalopts,
                    locale = bindists_tag.locale,
                    register = False,
                    toolchain_declarations = toolchain_declarations,
                )
                # all_bindists.extend(toolchain_declarations)

    _all_toolchains(
        name = "all_bindist_toolchains",
        toolchains = toolchain_declarations,
    )

haskell_toolchains = module_extension(
    implementation = _haskell_toolchains_impl,
    tag_classes = {
        "bindist": _bindist_tag,
        "bindists": _bindists_tag,
        # "bindists": _bindists_tag,
        # "override_existing":
        # ask for own toolchain:
    },
)
