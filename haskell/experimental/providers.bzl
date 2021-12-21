HaskellModuleInfo = provider(
    doc = "Information about a single Haskell module to compile",
    fields = {
        "attr": "The attributes of the haskell_module rule",
        "direct_module_deps": "The direct dependency targets of the haskell_module rule",
        "transitive_cross_library_dep_labels": "The labels of transitive cross-library dependencies of the module",
        "transitive_module_dep_labels": "List of the labels of transitive module dependencies of the haskell_module rule in the enclosing library",
    },
)
