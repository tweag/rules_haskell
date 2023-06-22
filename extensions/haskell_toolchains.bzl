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
    content = "\n".join(rctx.attr.toolchains)
    rctx.file("BUILD.bazel", content = content)

_all_toolchains = repository_rule(
    implementation = _all_toolchains_impl,
    attrs = {
        "toolchains": attr.string_list(),
    },
)

def _haskell_toolchains_impl(mctx):
    # Instead of creating one external repository for each `toolchain(...)` declaration,
    # we recover them in a list and declare them all in the `all_bindist_toolchains` repository.
    # In a way that respects
    # bazel's iteration order over modules.
    # toolchains declared by the root have priority

    # an alternative would be to use aliases to the (to get rid of the toolchain_declarations parameter)
    # if/once the following issue is resolved
    # https://github.com/bazelbuild/bazel/issues/16298
    toolchain_declarations = []
    toolchain_names = []
    found_bindists = False

    for module_index, module in enumerate(mctx.modules):
        for bindist_tag_index, bindist_tag in enumerate(module.tags.bindist):
            name = "bindist_{}_{}_{}".format(module.name, module.version, bindist_tag.name)
            if name in toolchain_names:
                fail(
                    """module "{module}~{version}" used the "bindist" tag twice with the "{tag_name}" name""".format(
                        tag_name = bindist_tag.name,
                        module = module.name,
                        version = module.version,
                    ),
                )
            else:
                toolchain_names.append(name)
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

        for bindists_tag in module.tags.bindists:
            # We only consider the first `bindists` tag, because subsequent
            # ones would have the same constraints and lower priority.
            if not found_bindists:
                found_bindists = True
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
