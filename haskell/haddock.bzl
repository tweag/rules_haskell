"""Haddock support"""

load (":path_utils.bzl", "module_name")
load (":set.bzl", "set")

load(":tools.bzl",
  "get_build_tools_path",
  "tools",
)

load(":providers.bzl",
     "HaskellPackageInfo",
     "HaddockInfo",
)

load("@bazel_skylib//:lib.bzl", "paths")

def _haskell_doc_aspect_impl(target, ctx):
  if HaskellPackageInfo not in target:
    return []

  pkg_id = "{0}-{1}".format(ctx.rule.attr.name, ctx.rule.attr.version)

  doc_dir = ctx.actions.declare_directory("doc-{0}".format(pkg_id))
  haddock_interface = ctx.actions.declare_file(
    paths.join(doc_dir.basename, "{0}.haddock".format(pkg_id)),

  )
  hoogle_file = ctx.actions.declare_file(
    paths.replace_extension(ctx.rule.attr.name, ".txt"),
    sibling = haddock_interface
  )

  args = ctx.actions.args()
  args.add([
    "-D", haddock_interface,
    "--package-name={0}".format(ctx.rule.attr.name),
    "--package-version={0}".format(ctx.rule.attr.version),
    "-o", doc_dir,
    "--html", "--hoogle",
    "--title={0}".format(pkg_id),
    "--hyperlinked-source",
  ])

  dep_interfaces = set.empty()
  for dep in ctx.rule.attr.deps:
    if HaddockInfo in dep:
      args.add("--read-interface={0},{1}".format(
        # Is this the best we can do? We have to tell haddock where the
        # docs for the given interface are and it has to be relative
        # because bazel moves these things around but is relying on
        # these always being in the same directory for the target the
        # right thing to do? Feels too hacky.
        paths.join("..", dep[HaddockInfo].doc_dir.basename),
        dep[HaddockInfo].interface_file.path
      ))
      dep_interfaces = set.mutable_insert(
        dep_interfaces,
        dep[HaddockInfo].interface_file
      )

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
    for f in input_sources
  ]

  self_outputs = [doc_dir, haddock_interface, hoogle_file] + static_haddock_outputs + module_htmls

  ctx.actions.run(
    inputs = depset(transitive = [
      set.to_depset(target[HaskellPackageInfo].caches),
      set.to_depset(target[HaskellPackageInfo].interface_files),
      set.to_depset(dep_interfaces),
      depset(input_sources),
    ]),
    outputs = self_outputs,
    progress_message = "Haddock {0}".format(ctx.rule.attr.name),
    executable = tools(ctx).haddock,
    arguments = [
      args,
      target[HaskellPackageInfo].haddock_ghc_args,
    ],
  )

  return [HaddockInfo(
    outputs = depset(self_outputs),
    interface_file = haddock_interface,
    doc_dir = doc_dir,
  )]

haskell_doc_aspect = aspect(
  _haskell_doc_aspect_impl,
  attr_aspects = ['deps'],
  toolchains = ["@io_tweag_rules_haskell//haskell:toolchain"],
  host_fragments = ["cpp"],
)

def _haskell_doc_rule_impl(ctx):
  interface_files = depset()
  for dep in ctx.attr.deps:
    if HaddockInfo in dep:
      interface_files = depset(transitive = [interface_files, dep[HaddockInfo].outputs])
  return [DefaultInfo(files = interface_files)]

haskell_doc = rule(
  _haskell_doc_rule_impl,
  attrs = {
    "deps": attr.label_list(
      aspects = [haskell_doc_aspect],
      doc = "List of Haskell libraries to generate documentation for.",
    ),
  },
  toolchains = ["@io_tweag_rules_haskell//haskell:toolchain"],
  host_fragments = ["cpp"],
)
"""Create API documentation.

Builds API documentation (using [Haddock][haddock]) for the given
Haskell libraries. It will automatically build documentation for any
transitive dependencies to allow for cross-package documentation
linking. Currently linking to `prebuilt_deps` is not supported.

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
