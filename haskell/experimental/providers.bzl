HaskellModuleInfo = provider(
    doc = "Information about a single compiled Haskell module.",
    fields = {
        "object_file": "The compiled `.o` file.",
        "interface_dir": "The import search directory for the interface file.",
        "interface_file": "The compiled `.hi` file.",
    },
)
