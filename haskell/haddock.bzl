"""Haddock suppport"""

load (":path_utils.bzl", "module_name")
load (":set.bzl", "set")

load(":tools.bzl",
  "get_haddock",
  "get_build_tools_path",
)

load(":providers.bzl", "HaskellPackageInfo")

load("@bazel_skylib//:lib.bzl", "paths")

HaddockInfo = provider(
  doc = "Haddock information.",
  fields = {
    "outputs": "All interesting outputs produced by Haddock.",
    "interface_file": "Haddock interface file.",
    "doc_dir": "Directory where all the documentation files live.",
  }
)

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
  input_sources = [ f for t in ctx.rule.attr.srcs for f in t.files.to_list() ]

  args = ctx.actions.args()
  args.add([
    "-D", haddock_interface,
    "--package-name={0}".format(ctx.rule.attr.name),
    "--package-version={0}".format(ctx.rule.attr.version),
    "-o", doc_dir,
    "--html", "--hoogle",
    # TODO: We shouldn't have to remember this, I think haskell
    # targets should expose all GHC flags they were ultimately built
    # with. That way we can pass exactly the same stuff. Having said
    # that, it's not that easy: only some flags are relevant and only
    # in some contexts.
    "--optghc=-hide-all-packages",
    "--title={0}".format(pkg_id),
    # This is absolutely required otherwise GHC doesn't what package
    # it's creating `Name`s for to put them in Haddock interface files
    # which then results in Haddock not being able to find names for
    # linking in environment after reading its interface file later.
    "--optghc=-this-unit-id", "--optghc={0}".format(pkg_id)
    # TODO: --hyperlinked-source or make a ticket
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

  # Expose all prebuilt packages
  for prebuilt_dep in ctx.rule.attr.prebuilt_dependencies:
    args.add("--optghc=-package {0}".format(prebuilt_dep))

  # Expose all bazel dependencies
  for pkg_name in target[HaskellPackageInfo].names.to_list():
    # Don't try to tell GHC to include the target itself.
    if target[HaskellPackageInfo].name != pkg_name:
      args.add("--optghc=-package {0}".format(pkg_name))

  # Include all package DBs we know of. Technically we shouldn't
  # include the one for target but it shouldn't hurt us.
  for pkg_cache in target[HaskellPackageInfo].caches.to_list():
    args.add("--optghc=-package-db {0}".format(pkg_cache.dirname))

  # Pass same flags the user set for the build.
  for ghc_flag in ctx.rule.attr.compiler_flags:
    args.add("--optghc={0}".format(ghc_flag))

  args.add(input_sources)

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
      target[HaskellPackageInfo].caches,
      set.to_depset(target[HaskellPackageInfo].interface_files),
      set.to_depset(dep_interfaces),
      depset(input_sources),
    ]),
    outputs = self_outputs,
    progress_message = "Haddock {0}".format(ctx.rule.attr.name),
    env = {
      "PATH": get_build_tools_path(ctx),
    },
    executable = get_haddock(ctx),
    arguments = [args],
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
  ````

[haddock]: http://haskell-haddock.readthedocs.io/en/latest/
"""
