HaskellModuleInfo = provider(
    doc = "Information about a single Haskell module to compile",
    fields = {
        "attr": "The attributes of the haskell_module rule",
        "transitive_module_deps": "The transitive dependency targets of the haskell_module rule",
    },
)
