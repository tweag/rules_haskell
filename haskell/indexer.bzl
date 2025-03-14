"""haskell-indexer support"""

# With a fair amount of copy-paste from haddock.bzl.

load("@bazel_skylib//lib:dicts.bzl", "dicts")
load("@bazel_skylib//lib:paths.bzl", "paths")
load(
    "@rules_haskell//haskell:providers.bzl",
    "HaskellInfo",
    "HaskellLibraryInfo",
    "HaskellToolchainLibraryInfo",
    "HaskellIndexerInfo",
    "get_ghci_extra_libs",
)
load(":private/context.bzl", "haskell_context", "render_env")
load(":private/set.bzl", "set")

def _get_named_path(suffix, unique_id):
    """Get path to a named file related to a package given its id.

    Args:
      suffix: string.
      unique_id: string.

    Returns:
      string: relative path to file.
    """
    return unique_id + "." + suffix

def _haskell_indexer_aspect_impl(target, ctx):
    if not (HaskellInfo in target):
        return []

    if HaskellToolchainLibraryInfo in target:
        return []

    hs = haskell_context(ctx, ctx.rule.attr)
    posix = ctx.toolchains["@rules_sh//sh/posix:toolchain_type"]

    is_lib = HaskellLibraryInfo in target
    unique_id = is_lib and target[HaskellLibraryInfo].package_id or hs.name

    # If the current package has no source file, just skip.
    if (target[HaskellInfo].source_files.to_list() == []):
        ctx.actions.do_nothing(mnemonic = "HaskellIndexer")
        return []

    entries_file = ctx.actions.declare_file(_get_named_path("kythe.entries", unique_id))
    log_file = ctx.actions.declare_file(_get_named_path("kythe.log", unique_id))

    args = ctx.actions.args()

    compile_flags = ctx.actions.args()
    compile_flags.add_all(target[HaskellInfo].compile_flags)
    compile_flags.add_all(target[HaskellInfo].source_files)

    locale_archive_depset = (
        depset([hs.toolchain.locale_archive]) if hs.toolchain.locale_archive != None else depset()
    )

    # C library dependencies for runtime.
    (ghci_extra_libs, ghc_env) = get_ghci_extra_libs(
        hs,
        posix,
        target[CcInfo],
        # TODO(robinp): can drop?
        # haddock changes directory during its execution. We prefix
        # LD_LIBRARY_PATH with the current working directory on wrapper script
        # startup.
        path_prefix = "$PWD",
    )

    indexer_wrapper = ctx.actions.declare_file("indexer_wrapper-{}".format(hs.name))
    ctx.actions.expand_template(
        template = ctx.file._indexer_wrapper_tpl,
        output = indexer_wrapper,
        substitutions = {
            "%{indexer}": ctx.file._indexer_binary.path,
            "%{outfile}": entries_file.path,
            "%{logfile}": log_file.path,
            # XXX Workaround
            # https://github.com/bazelbuild/bazel/issues/5980.
            "%{env}": render_env(dicts.add(hs.env, ghc_env)),
        },
        is_executable = True,
    )

    ctx.actions.run(
        inputs = depset(transitive = [
            target[HaskellInfo].package_databases,
            target[HaskellInfo].interface_dirs,
            target[HaskellInfo].source_files,
            target[HaskellInfo].extra_source_files,
            target[HaskellInfo].dynamic_libraries,

            # Seems needed for TH magiclink - why aren't the dynamic libs used
            # there?
            target[HaskellInfo].static_libraries,

            target[HaskellInfo].interface_dirs, # ??
            ghci_extra_libs,
            target[CcInfo].compilation_context.headers,
            depset([ctx.file._indexer_binary]),
            depset([
                hs.tools.ghc_pkg,
            ]),
            locale_archive_depset,
        ]),
        tools = [
          hs.tools.ghc,  # To get libdir
        ],
        outputs = [entries_file, log_file],
        mnemonic = "HaskellIndexer",
        progress_message = "HaskellIndexer {}".format(ctx.label),
        executable = indexer_wrapper,
        arguments = [
            args,
            compile_flags,
        ],
        env = {
            "PATH": (";" if hs.toolchain.is_windows else ":").join(posix.paths),
            "GHC": hs.tools.ghc.path,
            "CORPUS": ctx.workspace_name,
        },
    )

    indexer_info = HaskellIndexerInfo(
        entries = entries_file,
    )
    output_files = OutputGroupInfo(default = [entries_file, log_file])

    return [indexer_info, output_files]

haskell_indexer_aspect = aspect(
    _haskell_indexer_aspect_impl,
    attrs = {
        "_indexer_wrapper_tpl": attr.label(
            allow_single_file = True,
            default = Label("@rules_haskell//haskell:private/indexer_wrapper.sh.tpl"),
        ),
        # TODO(robinp): use a toolchain to get indexer binary
        "_indexer_binary": attr.label(
            allow_single_file = True,
            default = Label("@haskell_indexer_ghc//:bin/ghc_kythe_wrapper"),
        ),
    },
    attr_aspects = ["deps", "exports"],
    toolchains = [
        "@rules_haskell//haskell:toolchain",
        "@rules_sh//sh/posix:toolchain_type",
    ],
)

"""Output haskell-indexer entries in Kythe format.

Example usage:

    $ bazel build -c opt //... --aspects @rules_haskell//haskell:indexer.bzl%haskell_indexer_aspect
    $ find bazel-out/k8-opt/ -type f -name '*.kythe.entries'

Serving with Kythe:

    $ for i in $(find bazel-out/k8-opt/ -type f -name '*.entries'); do cp $i /tmp/entries/; done
    $ /opt/kythe-local/tools/write_tables -experimental_beam_pipeline -entries /tmp/entries/ -out /tmp/serving-table
    $ /opt/kythe-local/tools/http_server --serving_table /tmp/serving-table --listen 0.0.0.0:8080

"""

