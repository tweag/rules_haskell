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

def _haskell_doc_aspect_impl(target, ctx):
  if HaskellBuildInfo not in target or HaskellLibraryInfo not in target:
    return []

  pkg_id = target[HaskellLibraryInfo].package_id
  html_dir_raw = "doc-{0}".format(pkg_id)
  html_dir = ctx.actions.declare_directory(html_dir_raw)
  haddock_interface = ctx.actions.declare_file(
    paths.join(html_dir_raw, "{0}.haddock".format(pkg_id)),
  )
  hoogle_file = ctx.actions.declare_file(
    pkg_id + ".txt",
    sibling = haddock_interface,
  )

  args = ctx.actions.args()
  args.add([
    "-D", haddock_interface,
    "--package-name={0}".format(pkg_id),
    "--package-version={0}".format(ctx.rule.attr.version),
    "-o", html_dir,
    "--html", "--hoogle",
    "--title={0}".format(pkg_id),
    "--hyperlinked-source",
  ])

  dep_interfaces = set.empty()
  transitive_deps = {
    pkg_id: html_dir,
  }

  for dep in ctx.rule.attr.deps:
    if HaddockInfo in dep:
      args.add("--read-interface=../{0},{1}".format(
        dep[HaddockInfo].package_id,
        dep[HaddockInfo].interface_file.path
      ))
      dep_interfaces = set.mutable_insert(
        dep_interfaces,
        dep[HaddockInfo].interface_file
      )
      transitive_deps.update(dep[HaddockInfo].transitive_deps)

  static_haddock_outputs = [
    ctx.actions.declare_file(f, sibling=haddock_interface)
    for f in [
      "doc-index.html",
      "haddock-util.js",
      "hslogo-16.png",
      "index.html",
      "minus.gif",
      "ocean.css",
      "plus.gif",
      "synopsis.png"
    ]
  ]

  input_sources = [ f for t in ctx.rule.attr.srcs for f in t.files.to_list() ]

  module_htmls = [
    ctx.actions.declare_file(
      paths.replace_extension(
        module_name(ctx, f).replace('.', '-'),
        ".html"
      ),
      sibling=haddock_interface
    )
    for f in input_sources if f not in ctx.rule.files.hidden_haddock_modules
  ]

  self_outputs = [
    html_dir,
    haddock_interface,
    hoogle_file] + static_haddock_outputs + module_htmls

  ctx.actions.run(
    inputs = depset(transitive = [
      set.to_depset(target[HaskellBuildInfo].package_caches),
      set.to_depset(target[HaskellBuildInfo].interface_files),
      set.to_depset(target[HaskellBuildInfo].dynamic_libraries),
      set.to_depset(dep_interfaces),
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
    outputs = self_outputs,
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
    outputs = depset(self_outputs),
    interface_file = haddock_interface,
    html_dir = html_dir,
    package_id = pkg_id,
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
  interface_files = depset()
  all_caches = set.empty()

  for dep in ctx.attr.deps:
    if HaddockInfo in dep:
      interface_files = depset(transitive = [interface_files, dep[HaddockInfo].outputs])
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
