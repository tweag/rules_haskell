load("@rules_python//python:defs.bzl", "py_binary")

def cabal_wrapper(name, **kwargs):
    py_binary(
        name = name,
        srcs = [
            "@rules_haskell//haskell:private/cabal_wrapper.py",
            "@rules_haskell//haskell:private/generate_cabal_paths_module.py",
        ],
        srcs_version = "PY3",
        python_version = "PY3",
        imports = ["private"],
        **kwargs
    )
