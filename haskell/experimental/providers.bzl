HaskellModuleInfo = provider(
    doc = "Information about a single compiled Haskell module.",
    fields = {
        "import_dir": "The import search directory for the interface file. For example, if the interface file for `Some.Module` is stored under `bazel-out/k8-opt/bin/pkg/Some/Module.hi`, then `import_dir` should be `bazel-out/k8-opt/bin/pkg`.",
        "interface_file": "The compiled `.hi` file.",
        "dyn_interface_file": "The compiled `.dyn_hi` file.",
        "transitive_object_files": "A depset with the compiled `.o` and `.dyn_o` files of haskell_module dependencies.",
        "transitive_interface_files": "depset of pairs (import_dir, interface_file). A depset with the compiled `.hi` and `.dyn_hi` files of haskell_module dependencies.",
        "transitive_import_dirs": "The import search directories of interface files in transitive haskell_module dependencies.",
        "object_file": "The compiled `.o` file.",
        "dyn_object_file": "The compiled `.dyn_o` file.",
    },
)
