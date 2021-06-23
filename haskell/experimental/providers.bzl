HaskellModuleInfo = provider(
    doc = "Information about a single compiled Haskell module.",
    fields = {
        "import_dir": "The import search directory for the interface file. For example, if the interface file for `Some.Module` is stored under `bazel-out/k8-opt/bin/pkg/Some/Module.hi`, then `import_dir` should be `bazel-out/k8-opt/bin/pkg`.",
        "interface_file": "The compiled `.hi` file.",
        "object_file": "The compiled `.o` file.",
    },
)
