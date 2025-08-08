"""Derived context with Haskell-specific fields and methods"""

load("@bazel_skylib//lib:paths.bzl", "paths")
load("//haskell:providers.bzl", "all_dependencies_package_ids")
load(":private/path_utils.bzl", "join_path_list")

# buildifier: disable=name-conventions
# buildifier: disable=provider-params
HaskellContext = provider()

def append_to_path(env, is_windows, path_list):
    if path_list:
        if "PATH" in env and env["PATH"]:
            env["PATH"] = join_path_list(is_windows, [env["PATH"]] + path_list)
        else:
            env["PATH"] = join_path_list(is_windows, path_list)

def haskell_context(ctx, attr = None):
    toolchain = ctx.toolchains["@rules_haskell//haskell:toolchain"]

    if not attr:
        attr = ctx.attr

    deps = (attr.deps if hasattr(attr, "deps") else []) + (attr.exports if hasattr(attr, "exports") else []) + (attr.narrowed_deps if hasattr(attr, "narrowed_deps") else [])
    package_ids = all_dependencies_package_ids(deps)

    if hasattr(attr, "src_strip_prefix"):
        src_strip_prefix = attr.src_strip_prefix
    else:
        src_strip_prefix = ""

    src_root = paths.join(
        ctx.label.workspace_root,
        ctx.label.package,
        src_strip_prefix,
    )

    env = {
        "LANG": toolchain.locale,
        "RULES_HASKELL_GHC_PATH": toolchain.tools.ghc.path,
        "RULES_HASKELL_GHC_PKG_PATH": toolchain.tools.ghc_pkg.path,
        "RULES_HASKELL_LIBDIR_PATH": toolchain.libdir_path,
        "RULES_HASKELL_DOCDIR_PATH": toolchain.docdir_path,
    }

    if toolchain.locale_archive != None:
        env["LOCALE_ARCHIVE"] = toolchain.locale_archive.path

    coverage_enabled = False
    if hasattr(ctx, "configuration"):
        coverage_enabled = ctx.configuration.coverage_enabled

    ghc_wrapper = None
    if hasattr(ctx.executable, "_ghc_wrapper"):
        ghc_wrapper = ctx.executable._ghc_wrapper

    worker = getattr(ctx.executable, "worker", None)

    return HaskellContext(
        # Fields
        name = attr.name,
        label = ctx.label,
        toolchain = toolchain,
        tools = toolchain.tools,
        ghc_wrapper = ghc_wrapper,
        worker = worker,
        package_ids = package_ids,
        src_root = src_root,
        package_root = paths.join(ctx.label.workspace_root, ctx.label.package),
        env = env,
        mode = ctx.var["COMPILATION_MODE"],
        actions = ctx.actions,
        bin_dir = ctx.bin_dir,
        genfiles_dir = ctx.genfiles_dir,
        coverage_enabled = coverage_enabled,
        features = struct(
            fully_static_link = "fully_static_link" in ctx.features,
        ),
        tools_config = toolchain.tools_config,
    )

def render_env(env):
    """Render environment dict to shell exports.

    ### Examples

      >>> render_env({"PATH": "foo:bar", "LANG": "lang"})
      export PATH="foo:bar"
      export LANG="lang"

    """
    return "\n".join([
        'export {}="{}"'.format(k, v)
        for k, v in env.items()
    ])
