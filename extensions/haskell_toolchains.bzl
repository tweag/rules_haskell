""" Module extension to install bindist haskell toolchains"""

load(
    "@rules_haskell//haskell:ghc_bindist.bzl",
    "ghc_bindist",
    "ghc_bindist_toolchain_declaration",
    "ghc_bindists_toolchain_declarations",
    "haskell_register_ghc_bindists",
)

_bindists_tag = tag_class(
    attrs = {
        "version": attr.string(
            doc = "[see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-version)",
        ),
        "ghcopts": attr.string_list(
            doc = "[see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-ghcopts)",
        ),
        "haddock_flags": attr.string_list(
            doc = "haddock_flags: [see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-haddock_flags)",
        ),
        "repl_ghci_args": attr.string_list(
            doc = "[see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-repl_ghci_args)",
        ),
        "cabalopts": attr.string_list(
            doc = "[see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-cabalopts)",
        ),
        "locale": attr.string(
            doc = "[see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-locale)",
        ),
    },
    doc = """See [rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains).
    Only the first `bindists` tag is taken into account (according to the iteration order over modules).
    """,
)

_bindist_tag = tag_class(
    attrs = {
        "version": attr.string(
            mandatory = True,
            doc = "The desired GHC version",
        ),
        "target": attr.string(
            mandatory = True,
            doc = "The desired architecture (See [ghc_bindist_generated.json](https://github.com/tweag/rules_haskell/blob/master/haskell/private/ghc_bindist_generated.json))",
        ),
        "ghcopts": attr.string_list(
            doc = "[see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-ghcopts)",
        ),
        "haddock_flags": attr.string_list(
            doc = "[see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-haddock_flags)",
        ),
        "repl_ghci_args": attr.string_list(
            doc = "[see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-repl_ghci_args) ",
        ),
        "cabalopts": attr.string_list(
            doc = "[see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-cabalopts)",
        ),
        "locale": attr.string(
            doc = "[see rules_haskell_toolchains](toolchain.html#rules_haskell_toolchains-locale)",
        ),
    },
    doc = "Declares and configure a bindist haskell toolchain. See [ghc_bindist](ghc_bindist.html#ghc_bindist).",
)

def _left_pad_zero(index, length):
    if index < 0:
        fail("index must be non-negative")
    return ("0" * length + str(index))[-length:]

def _all_toolchains_impl(rctx):
    # In the same BUILD file, toolchain are registered in alphabetical order, so we use a padded index in the toolchain name.
    # https://github.com/bazelbuild/rules_go/blob/e116651f92a15f4e5dd63ea09dd9ae2b2dd8e2f3/go/private/extensions.bzl#L135-L139
    pad_size = len(str(len(rctx.attr.toolchains)))
    toolchains = [
        f.format("toolchain_{}".format(_left_pad_zero(i, pad_size)))
        for i, f in enumerate(rctx.attr.toolchains)
    ]
    content = "\n".join(toolchains)
    rctx.file("BUILD.bazel", content = content)

all_toolchains = repository_rule(
    implementation = _all_toolchains_impl,
    attrs = {
        "toolchains": attr.string_list(
            doc = "All the `toolchain(...)` declarations for the bindist toolchains as string",
        ),
    },
    doc = """ Used to generate the `all_bindist_toolchains` external repository.
    We can then invoke `register_toolchains("@all_bindist_toolchains//:all")` in the MODULE.bazel file.
    """,
)

def _haskell_toolchains_impl(mctx):
    # We gather the declarations of all the toolchains in the `toolchain_declarations` list
    # in order to write them to the `@all_bindist_toolchains` repository.

    # The code could be simplified a bit by registering aliases to the
    # `toolchain` rules, once the following issue is resolved
    # https://github.com/bazelbuild/bazel/issues/16298
    toolchain_declarations = []

    found_bindists = False
    for module in mctx.modules:
        bindist_targets = []
        for bindist_tag in module.tags.bindist:
            name = "bindist_{}_{}_{}".format(module.name, module.version, bindist_tag.target)
            if bindist_tag.target in bindist_targets:
                fail(
                    """Module "{module}~{version}" used the "bindist" tag twice with the "{target}" target.""".format(
                        target = bindist_tag.target,
                        module = module.name,
                        version = module.version,
                    ),
                )
            else:
                bindist_targets.append(bindist_tag.target)
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
            )
            toolchain_declarations.append(
                ghc_bindist_toolchain_declaration(
                    target = bindist_tag.target,
                    bindist_name = name,
                    toolchain_name = "{}",  # We wait before chosing the toolchain name in the all_toolchain rule.
                ),
            )

        if len(module.tags.bindists) > 1:
            fail(
                """Module "{module}~{version}" used the "bindists" tag more than once.""".format(
                    module = module.name,
                    version = module.version,
                ),
            )

        if module.tags.bindists and not found_bindists:
            # We only consider the first `bindists` tag accross all modules, because subsequent
            # ones would have the same constraints and lower priority.
            found_bindists = True
            bindists_tag = module.tags.bindists[0]
            haskell_register_ghc_bindists(
                version = bindists_tag.version,
                ghcopts = bindists_tag.ghcopts,
                haddock_flags = bindists_tag.haddock_flags,
                repl_ghci_args = bindists_tag.repl_ghci_args,
                cabalopts = bindists_tag.cabalopts,
                locale = bindists_tag.locale,
                register = False,
            )
            toolchain_declarations.extend(
                ghc_bindists_toolchain_declarations(mctx, bindists_tag.version),
            )

    all_toolchains(
        name = "all_bindist_toolchains",
        toolchains = toolchain_declarations,
    )

haskell_toolchains = module_extension(
    implementation = _haskell_toolchains_impl,
    tag_classes = {
        "bindist": _bindist_tag,
        "bindists": _bindists_tag,
    },
)
