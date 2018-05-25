"""Linting"""

load(":cc.bzl", "cc_headers")
load(":private/context.bzl", "haskell_context")
load(":private/set.bzl", "set")
load(":private/path_utils.bzl",
     "target_unique_name"
)
load(":private/tools.bzl",
  "get_build_tools_path",
  "tools",
)
load(":private/providers.bzl",
     "HaskellBuildInfo",
     "HaskellLibraryInfo",
     "HaskellBinaryInfo",
     "HaskellLintInfo",
)

def _collect_lint_logs(deps):
  lint_logs = set.empty()
  for dep in deps:
    if HaskellLintInfo in dep:
      set.mutable_union(lint_logs, dep[HaskellLintInfo].outputs)
  return lint_logs

def _haskell_lint_rule_impl(ctx):
  return [DefaultInfo(
    files = set.to_depset(_collect_lint_logs(ctx.attr.deps)),
  )]

def _haskell_lint_aspect_impl(target, ctx):
  hs = haskell_context(ctx, ctx.rule.attr)

  if HaskellBuildInfo not in target:
    return []

  build_info = target[HaskellBuildInfo]
  lib_info = target[HaskellLibraryInfo] if HaskellLibraryInfo in target else None
  bin_info = target[HaskellBinaryInfo] if HaskellBinaryInfo in target else None

  args = ctx.actions.args()

  args.add([
    "-O0",
    "-v0",
    "-fno-code",
    "-Wall",
    "-Werror",
    "-Wcompat",
    "-Wincomplete-record-updates",
    "-Wincomplete-uni-patterns",
    "-Wredundant-constraints",
    "-Wnoncanonical-monad-instances",
    "--make",
    "-hide-all-packages",
  ])

  # Expose all prebuilt dependencies
  for prebuilt_dep in set.to_list(build_info.prebuilt_dependencies):
    args.add(["-package", prebuilt_dep])

  # Expose all bazel dependencies
  for package in set.to_list(build_info.package_ids):
    if lib_info == None or package != lib_info.package_id:
      args.add(["-package-id", package])

  for cache in set.to_list(build_info.package_caches):
    args.add(["-package-db", cache.dirname])

  sources = set.to_list(
    lib_info.source_files if lib_info != None else bin_info.source_files
  )

  args.add(sources)

  lint_log = ctx.actions.declare_file(
    target_unique_name(hs, "lint-log")
  )

  lint_logs = _collect_lint_logs(
    ctx.rule.attr.deps
  )

  ctx.actions.run_shell(
    inputs = depset(transitive = [
      depset(sources),
      set.to_depset(build_info.package_confs),
      set.to_depset(build_info.package_caches),
      set.to_depset(build_info.interface_files),
      set.to_depset(build_info.dynamic_libraries),
      depset(build_info.external_libraries.values()),
      depset([
        hs.tools.ghc,
        hs.tools.cat,
      ]),
    ]),
    outputs = [lint_log],
    progress_message = "Linting {0}".format(ctx.rule.attr.name),
    command = """
    (ghc "$@" > {output} 2>&1) || (last_code=$? && cat {output} && exit $last_code)
    """.format(
      ghc = tools(ctx).ghc.path,
      output = lint_log.path,
    ),
    arguments = [args],
    env = hs.env,
  )

  return [HaskellLintInfo(
    outputs = set.mutable_insert(lint_logs, lint_log)
  )]

haskell_lint_aspect = aspect(
  _haskell_lint_aspect_impl,
  attr_aspects = ["deps"],
  toolchains = ["@io_tweag_rules_haskell//haskell:toolchain"],
)

haskell_lint = rule(
  _haskell_lint_rule_impl,
  attrs = {
    "deps": attr.label_list(
      aspects = [haskell_lint_aspect],
      doc = "List of Haskell targets to lint.",
    ),
  },
  toolchains = ["@io_tweag_rules_haskell//haskell:toolchain"],
)
"""Check source code of targets in `deps` using a restrictive set of GHC
flags.

The following flags will be used:

* `-Wall`
* `-Werror`
* `-Wcompat`
* `-Wincomplete-record-updates`
* `-Wincomplete-uni-patterns`
* `-Wredundant-constraints`
"""

def _haskell_doctest_aspect_impl(target, ctx):
  hs = haskell_context(ctx, ctx.rule.attr)

  if HaskellBuildInfo not in target:
    return []

  build_info = target[HaskellBuildInfo]
  lib_info = target[HaskellLibraryInfo] if HaskellLibraryInfo in target else None
  bin_info = target[HaskellBinaryInfo] if HaskellBinaryInfo in target else None

  args = ctx.actions.args()

  args.add([
    "--no-magic",
    "-hide-all-packages",
  ])

  # Expose all prebuilt dependencies
  for prebuilt_dep in set.to_list(build_info.prebuilt_dependencies):
    args.add(["-package", prebuilt_dep])

  # Expose all bazel dependencies
  for package in set.to_list(build_info.package_ids):
    if lib_info != None or package != lib_info.package_id:
      args.add(["-package-id", package])

  for cache in set.to_list(build_info.package_caches):
    args.add(["-package-db", cache.dirname])

  sources = set.to_list(
    lib_info.source_files if lib_info != None else bin_info.source_files
  )

  args.add(sources)

  lint_log = ctx.actions.declare_file(
    target_unique_name(hs, "doctest-log")
  )

  lint_logs = _collect_lint_logs(
    ctx.rule.attr.deps
  )

  ctx.actions.run_shell(
    inputs = depset(transitive = [
      depset(sources),
      set.to_depset(build_info.package_confs),
      set.to_depset(build_info.package_caches),
      set.to_depset(build_info.interface_files),
      set.to_depset(build_info.dynamic_libraries),
      depset(build_info.external_libraries.values()),
      depset([
        hs.tools.doctest,
        hs.tools.cat,
      ]),
    ]),
    outputs = [lint_log],
    progress_message = "Doctesting {0}".format(ctx.rule.attr.name),
    command = """
    (doctest "$@" > {output} 2>&1) || (last_code=$? && cat {output} && exit $last_code)
    """.format(output = lint_log.path),
    arguments = [args],
    env = hs.env,
  )

  return [HaskellLintInfo(
    outputs = set.mutable_insert(lint_logs, lint_log)
  )]

haskell_doctest_aspect = aspect(
  _haskell_doctest_aspect_impl,
  attr_aspects = ["deps"],
  toolchains = ["@io_tweag_rules_haskell//haskell:toolchain"],
)

haskell_doctest = rule(
  _haskell_lint_rule_impl,
  attrs = {
    "deps": attr.label_list(
      aspects = [haskell_doctest_aspect],
      doc = "List of Haskell targets to lint.",
    ),
  },
  toolchains = ["@io_tweag_rules_haskell//haskell:toolchain"],
)
"""Run doctest test on targets in `deps`.

Note that your toolchain must be equipped with `doctest` executable, i.e.
you should specify location of the executable using the `doctest` attribute
when you invoke `haskell_toolchain`.
"""
