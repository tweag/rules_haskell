"""Haddock support"""

load (":path_utils.bzl", "module_name")
load (":set.bzl", "set")

load(":actions.bzl",
  "get_pkg_name",
)

load(":tools.bzl",
  "get_build_tools_path",
  "tools",
)

load(":providers.bzl",
     "HaskellBuildInfo",
     "HaskellLibraryInfo",
     "HaddockInfo",
)

load("@bazel_skylib//:lib.bzl", "paths")

def _get_haddock_path(html_dir, package_id):
  """TODO
  """
  return paths.join(
    html_dir.path,
    package_id + ".haddock",
  )

def _haskell_doc_aspect_impl(target, ctx):
  if HaskellBuildInfo not in target or HaskellLibraryInfo not in target:
    return []

  package_id = target[HaskellLibraryInfo].package_id
  html_dir_raw = "doc-{0}".format(package_id)
  html_dir = ctx.actions.declare_directory(html_dir_raw)

  args = ctx.actions.args()
  args.add([
    "-D", _get_haddock_path(html_dir, package_id),
    "--package-name={0}".format(package_id),
    "--package-version={0}".format(ctx.rule.attr.version),
    "-o", html_dir,
    "--html", "--hoogle",
    "--title={0}".format(package_id),
    "--hyperlinked-source",
  ])

  dep_html_dirs = set.empty()
  transitive_deps = {
    package_id: html_dir,
  }

  for dep in ctx.rule.attr.deps:
    if HaddockInfo in dep:
      args.add("--read-interface=../{0},{1}".format(
        dep[HaddockInfo].package_id,
        _get_haddock_path(
          dep[HaddockInfo].html_dir,
          dep[HaddockInfo].package_id
        ),
      ))
      dep_html_dirs = set.mutable_insert(
        dep_html_dirs,
        dep[HaddockInfo].html_dir
      )
      transitive_deps.update(dep[HaddockInfo].transitive_deps)

  input_sources = [ f for t in ctx.rule.attr.srcs for f in t.files.to_list() ]

  ctx.actions.run(
    inputs = depset(transitive = [
      set.to_depset(target[HaskellBuildInfo].package_caches),
      set.to_depset(target[HaskellBuildInfo].interface_files),
      set.to_depset(target[HaskellBuildInfo].dynamic_libraries),
      set.to_depset(dep_html_dirs),
      # Need to give source files this way because the source_files field of
      # HaskellLibraryInfo provider contains files that are already
      # pre-processed by hsc2hs and these should be visible to Haddock.
      set.to_depset(target[HaskellLibraryInfo].source_files),
      set.to_depset(target[HaskellLibraryInfo].header_files),
      depset([
        tools(ctx).ghc_pkg,
        tools(ctx).haddock,
      ]),
    ]),
    outputs = [html_dir],
    progress_message = "Haddock {0}".format(ctx.rule.attr.name),
    executable = ctx.file._haddock_wrapper,
    arguments = [
      args,
      target[HaskellLibraryInfo].haddock_args,
    ],
    env = {
      "RULES_HASKELL_GHC_PKG": tools(ctx).ghc_pkg.path,
      "RULES_HASKELL_HADDOCK": tools(ctx).haddock.path,
      "RULES_HASKELL_PREBUILT_DEPS": " ".join(
        set.to_list(target[HaskellBuildInfo].prebuilt_dependencies)
      ),
    },
  )

  return [HaddockInfo(
    package_id = package_id,
    html_dir = html_dir,
    transitive_deps = transitive_deps,
  )]

haskell_doc_aspect = aspect(
  _haskell_doc_aspect_impl,
  attrs = {
    "_haddock_wrapper": attr.label(
      allow_single_file = True,
      default = Label("@io_tweag_rules_haskell//haskell:haddock-wrapper.sh"),
    ),
  },
  attr_aspects = ['deps'],
  toolchains = ["@io_tweag_rules_haskell//haskell:toolchain"],
)

def _haskell_doc_rule_impl(ctx):

  # Reject cases when number of dependencies is 0.

  if not ctx.attr.deps:
    fail("haskell_doc needs at least one haskell_library component in deps")

  doc_root_raw = ctx.attr.name
  deps_dict_original = {}
  # interface_files = depset()
  all_caches = set.empty()

  for dep in ctx.attr.deps:
    if HaddockInfo in dep:
      # interface_files = depset(transitive = [interface_files, dep[HaddockInfo].outputs])
      deps_dict_original.update(dep[HaddockInfo].transitive_deps)
    if HaskellBuildInfo in dep:
      set.mutable_union(
        all_caches,
        dep[HaskellBuildInfo].package_caches
      )

  # Copy docs of Bazel deps into predefined locations under the root doc
  # directory.

  deps_dict_copied = {}
  doc_root_path = ""

  print(deps_dict_original)

  for package_id in deps_dict_original:
    html_dir = deps_dict_original[package_id]
    output_dir = ctx.actions.declare_directory(
      paths.join(
        doc_root_raw,
        package_id,
      )
    )
    doc_root_path = paths.dirname(output_dir.path)

    deps_dict_copied[package_id] = output_dir

    ctx.actions.run(
      inputs = [
        tools(ctx).cp,
        tools(ctx).mkdir,
        html_dir,
      ],
      outputs = [output_dir],
      executable = ctx.file._copy_dep_haddock,
      env = {
        "RULES_HASKELL_CP": tools(ctx).cp.path,
        "RULES_HASKELL_MKDIR": tools(ctx).mkdir.path,
        "RULES_HASKELL_HTML_DIR": html_dir.path,
        "RULES_HASKELL_DOC_DIR": doc_root_path,
        "RULES_HASKELL_TARGET_DIR": output_dir.path,
      },
    )

  # Do one more Haddock call to generate the unified index

  args = ctx.actions.args()
  args.add([
    "-o", doc_root_path,
    "--title={0}".format(ctx.attr.name),
    "--gen-index",
    "--gen-contents",
  ])

  if ctx.attr.index_transitive_deps:
    # Include all packages in the unified index.
    for package_id in deps_dict_copied:
      copied_html_dir = deps_dict_copied[package_id].path
      args.add("--read-interface=./{0},{1}".format(
        package_id,
        paths.join(
          copied_html_dir,
          package_id + ".haddock",
        )
      ))
  else:
    # Include only direct dependencies.
    for dep in ctx.attr.deps:
      if HaddockInfo in dep:
        package_id = dep[HaddockInfo].package_id
        copied_html_dir = deps_dict_copied[package_id].path
        args.add("--read-interface=./{0},{1}".format(
          package_id,
          paths.join(
            copied_html_dir,
            package_id + ".haddock",
          )
        ))

  for cache in set.to_list(all_caches):
    args.add(["--optghc=-package-db={0}".format(cache.dirname)])

  static_haddock_outputs = [
    ctx.actions.declare_file(paths.join(doc_root_raw, f))
    for f in [
      "doc-index.html",
      "haddock-util.js",
      "hslogo-16.png",
      "index.html",
      "minus.gif",
      "ocean.css",
      "plus.gif",
      "synopsis.png",
    ]
  ]

  ctx.actions.run(
    inputs = depset(transitive = [
      set.to_depset(all_caches),
      depset(deps_dict_copied.values()),

    ]),
    outputs = static_haddock_outputs,
    progress_message = "Creating unified Haddock index {0}".format(ctx.attr.name),
    executable = tools(ctx).haddock,
    arguments = [args],
  )

  return [DefaultInfo(
    files = depset(deps_dict_copied.values() + static_haddock_outputs),
  )]

haskell_doc = rule(
  _haskell_doc_rule_impl,
  attrs = {
    "deps": attr.label_list(
      aspects = [haskell_doc_aspect],
      doc = "List of Haskell libraries to generate documentation for.",
    ),
    "index_transitive_deps": attr.bool(
      default = False,
      doc = "Whether to include documentation of transitive dependencies in index.",
    ),
    "_copy_dep_haddock": attr.label(
      allow_single_file = True,
      default = Label("@io_tweag_rules_haskell//haskell:copy-dep-haddock.sh"),
    ),
  },
  toolchains = ["@io_tweag_rules_haskell//haskell:toolchain"],
)
"""Create API documentation.

Builds API documentation (using [Haddock][haddock]) for the given
Haskell libraries. It will automatically build documentation for any
transitive dependencies to allow for cross-package documentation
linking.

Example:
  ```bzl
  haskell_library(
    name = "my-lib",
    ...
  )

  haskell_doc(
    name = "my-lib-doc",
    deps = [":my-lib"],
  )
  ```

[haddock]: http://haskell-haddock.readthedocs.io/en/latest/
"""
