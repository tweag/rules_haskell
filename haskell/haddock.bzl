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

def _truly_relativize(target, relative_to):
  """Return a relative path to `target` from `relative_to`.

  Args:
    target: File, path to directory we want to get relative path to.
    relative_to: File, path to directory from which we are starting.

  Returns:
    string: relative path to `target`.
  """
  t_pieces = target.path.split('/')
  r_pieces = relative_to.path.split('/')
  common_part_len = 0

  for tp, rp in zip(t_pieces, r_pieces):
    if tp == rp:
      common_part_len += 1
    else:
      break

  result = [".."] * (len(r_pieces) - common_part_len)
  result += t_pieces[common_part_len:]

  return "/".join(result)

def _haskell_doc_aspect_impl(target, ctx):
  if HaskellBuildInfo not in target or HaskellLibraryInfo not in target:
    return []

  pkg_id = target[HaskellLibraryInfo].package_id

  doc_dir_raw = "doc-{0}".format(pkg_id)

  doc_dir = ctx.actions.declare_directory(doc_dir_raw)
  haddock_interface = ctx.actions.declare_file(
    paths.join(doc_dir_raw, "{0}.haddock".format(pkg_id)),

  )
  hoogle_file = ctx.actions.declare_file(
    pkg_id + ".txt",
    sibling = haddock_interface
  )

  args = ctx.actions.args()
  args.add([
    "-D", haddock_interface,
    "--package-name={0}".format(pkg_id),
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
        _truly_relativize(
          dep[HaddockInfo].doc_dir,
          doc_dir,
          ),
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

  self_outputs = [
    doc_dir,
    haddock_interface,
    hoogle_file] + static_haddock_outputs + module_htmls

  ctx.actions.run(
    inputs = depset(transitive = [
      set.to_depset(target[HaskellBuildInfo].package_caches),
      set.to_depset(target[HaskellBuildInfo].interface_files),
      set.to_depset(target[HaskellBuildInfo].dynamic_libraries),
      set.to_depset(dep_interfaces),
      depset(input_sources),
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
    doc_dir = doc_dir,
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
