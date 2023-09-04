HaskellModuleInfo = provider(
    doc = "Information about a single Haskell module to compile",
    fields = {
        "attr": "The attributes of the haskell_module rule",
        "direct_module_deps": "The direct dependency targets of the haskell_module rule",
        "direct_cross_library_deps": "The direct cross-library dependency targets of the haskell_module rule",
        "transitive_module_dep_labels": "List of the labels of transitive module dependencies of the haskell_module rule in the enclosing library",
    },
)
