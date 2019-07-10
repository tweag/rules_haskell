"""Derived context with Haskell-specific fields and methods"""

load("@bazel_skylib//lib:paths.bzl", "paths")
load("@io_tweag_rules_haskell//haskell:providers.bzl", "HaskellLibraryInfo")

HaskellContext = provider()

def haskell_context(ctx, attr = None):
    toolchain = ctx.toolchains["@io_tweag_rules_haskell//haskell:toolchain"]

    if not attr:
        attr = ctx.attr

    if hasattr(attr, "deps"):
        package_ids = [
            dep[HaskellLibraryInfo].package_id
            for dep in attr.deps
            if HaskellLibraryInfo in dep
        ]
    else:
        package_ids = []

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
    }

    if toolchain.locale_archive != None:
        env["LOCALE_ARCHIVE"] = toolchain.locale_archive.path

    coverage_enabled = False
    if hasattr(ctx, "configuration"):
        coverage_enabled = ctx.configuration.coverage_enabled

    worker = None
    if hasattr(ctx.executable, "_worker"):
        worker = ctx.executable._worker

    return HaskellContext(
        # Fields
        name = attr.name,
        label = ctx.label,
        toolchain = toolchain,
        tools = toolchain.tools,
        worker = worker,
        package_ids = package_ids,
        src_root = src_root,
        package_root = ctx.label.workspace_root + ctx.label.package,
        env = env,
        mode = ctx.var["COMPILATION_MODE"],
        actions = ctx.actions,
        bin_dir = ctx.bin_dir,
        genfiles_dir = ctx.genfiles_dir,
        coverage_enabled = coverage_enabled,
    )

def render_env(env):
    """Render environment dict to shell exports.

    Example:

      >>> render_env({"PATH": "foo:bar", "LANG": "lang"})
      export PATH=foo:bar
      export LANG=lang

    """
    return "\n".join([
        "export {}={}".format(k, v)
        for k, v in env.items()
    ])
