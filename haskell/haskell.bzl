HASKELL_FILETYPE = FileType([".hs"])

# TODO
haskell_library = 1

_haskell_common_attrs = {
    "srcs": attr.label_list(allow_files = HASKELL_FILETYPE),
    "deps": attr.label_list(),
}

def _haskell_binary_impl(ctx):
    haskell_binary = ctx.outputs.executable

    compile_inputs = ctx.files.srcs

    ctx.actions.run(
        inputs = compile_inputs,
        outputs = [haskell_binary],
        mnemonic = "Ghc",
        executable = "ghc",
        arguments = [
            "-o",
            haskell_binary.path,
        ] + [
            src.path for src in compile_inputs
        ],
        use_default_shell_env = True,
        progress_message = ("Compiling Haskell binary %s (%d files)"
                            % (ctx.label.name, len(ctx.files.srcs))))

    return struct(haskell_srcs = ctx.files.srcs,
                  haskell_deps = ctx.attr.deps)

haskell_binary = rule(
    _haskell_binary_impl,
    attrs = _haskell_common_attrs,
    executable = True,
)
