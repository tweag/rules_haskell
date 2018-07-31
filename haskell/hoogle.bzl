"""Hoogle support"""

load(
    ":private/providers.bzl",
    "CombinedHaddockInfo",
    "HaskellBinaryInfo",
    "HaskellBuildInfo",
    "HaskellLibraryInfo",
)
load(":private/context.bzl", "haskell_context")
load(":private/set.bzl", "set")
load(
    ":private/path_utils.bzl",
    "get_external_libs_path",
    "get_lib_name",
    "target_unique_name",
)
load("@bazel_skylib//:lib.bzl", "dicts", "paths")

HoogleDb = provider(
    fields = {
        "db_file": "Hoogle database file",
    },
)

def _hoogle_toolchain_impl(ctx):
    return platform_common.ToolchainInfo(
        name = ctx.label.name,
        hoogle = ctx.files.hoogle,
    )

_hoogle_toolchain = rule(
    _hoogle_toolchain_impl,
    attrs = {
        "hoogle": attr.label(
            doc = "Hoogle executable",
            cfg = "host",
            executable = True,
            single_file = True,
            mandatory = True,
        ),
    },
)

def haskell_hoogle_toolchain(name, hoogle, **kwargs):
    impl_name = name + "-impl"
    _hoogle_toolchain(
        name = impl_name,
        hoogle = hoogle,
        visibility = ["//visibility:public"],
        **kwargs
    )
    native.toolchain(
        name = name,
        toolchain_type = "@io_tweag_rules_haskell//haskell:hoogle-toolchain",
        toolchain = ":" + impl_name,
    )

def _hoogle_aspect_impl(target, ctx):
    if CombinedHaddockInfo not in target:
      return []

    hs = haskell_context(ctx, ctx.rule.attr)
    hoogle_toolchain = ctx.toolchains["@io_tweag_rules_haskell//haskell:hoogle-toolchain"]

    haddock_info = target[CombinedHaddockInfo]

    real_hoogle_arg = ctx.actions.args()
    real_hoogle_arg.add(hoogle_toolchain.hoogle[0].path)

    prebuilt_deps_list = []
    for dep in ctx.rule.attr.deps:
      if HaskellBuildInfo in dep:
          prebuilt_deps_list.extend(set.to_list(dep[HaskellBuildInfo].prebuilt_dependencies))
    prebuilt_deps = ctx.actions.args()
    prebuilt_deps.add_all(depset(prebuilt_deps_list))
    prebuilt_deps = ctx.actions.args()
    prebuilt_deps.use_param_file(param_file_arg = "%s", use_always = True)

    db_file = ctx.actions.declare_file("hoogle.hoo")
    args = ctx.actions.args()
    args.add("generate")
    for package_id in haddock_info.html_dirs:
      args.add("--local={0}".format(haddock_info.html_dirs[package_id].path))
    args.add("--database={0}".format(db_file.path))
    ctx.actions.run(
      inputs = depset(transitive = [
          depset(haddock_info.html_dirs.values()),
          depset([
              hs.tools.xargs,
              hs.tools.ghc_pkg,
              hs.tools.bash,
              ctx.file._hoogle_wrapper,
          ] + hoogle_toolchain.hoogle),
      ]
      ),
      outputs = [db_file],
      mnemonic = "HaskellHoogleDB",
      executable = ctx.file._hoogle_wrapper,
      arguments = [real_hoogle_arg, prebuilt_deps, args],
      env = hs.env,
    )
    return [HoogleDb(
        db_file = depset([db_file]),
    )]

hoogle_aspect = aspect(
    _hoogle_aspect_impl,
    attrs = {
        "_hoogle_wrapper": attr.label(
            allow_single_file = True,
            default = Label("@io_tweag_rules_haskell//haskell:private/hoogle_wrapper.sh"),
          ),
    },
    attr_aspects = ["deps"],
    toolchains = [
        "@io_tweag_rules_haskell//haskell:toolchain",
        "@io_tweag_rules_haskell//haskell:hoogle-toolchain",
    ],
)

def _hoogle_impl(ctx):
    dbs = []

    for dep in ctx.attr.deps:
        dbs.append(dep[HoogleDb].db_file)

    return DefaultInfo(
        files = depset([], transitive = dbs),
    )

hoogle = rule(
    _hoogle_impl,
    attrs = {
        "deps": attr.label_list(
            aspects = [hoogle_aspect],
            doc = "List of haddock targets to generate hoogle for.",
          ),
    },
)
"""Run `hoogle generate` on targets in `deps`
"""
