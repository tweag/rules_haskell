load(":providers.bzl", "HaskellPackageInfo")

load(":set.bzl", "set")

# XXX this provider shouldn't be necessary. But since Skylark rules
# can neither return CcSkylarkApiProvider nor properly test for its
# existence in a dependency, we're forced to introduce this hack for
# now. See https://github.com/bazelbuild/bazel/issues/4370.
CcSkylarkApiProviderHacked = provider(
  doc = "Skylark emulation of CcSkylarkApiProvider. Temporary hack.",
  fields = {
    "transitive_headers": """
      Returns a depset of headers that have been declared in the src
      or headers attribute(possibly empty but never None).
    """
  },
)

def cc_headers(ctx):
  # Bring in scope the header files of dependencies, if any.
  hdrs = [
    hdr
    # XXX There's gotta be a better way to test the presence of
    # CcSkylarkApiProvider.
    for dep in ctx.attr.deps if hasattr(dep, "cc")
    for hdr in dep.cc.transitive_headers.to_list()
  ] + [
    hdr
    # XXX cc_import doesn't produce a cc field, so we emulate it with
    # a custom provider.
    for dep in ctx.attr.deps if CcSkylarkApiProviderHacked in dep
    for hdr in dep[CcSkylarkApiProviderHacked].transitive_headers.to_list()
  ]
  return hdrs, ["-I" + hdr.dirname for hdr in hdrs]

def _cc_import_impl(ctx):
  return [
    DefaultInfo(files = depset(ctx.attr.shared_library.files)),
    CcSkylarkApiProviderHacked(transitive_headers =
      depset(transitive = [l.files for l in ctx.attr.hdrs]),
    )
  ]

# XXX This is meant as a drop-in replacement for the native cc_import,
# but it's a temporary hack. It's only necessary because the native
# cc_import does not provide CcSkylarkApiProvider. So we write our own
# rule that does just that. See
# https://github.com/bazelbuild/bazel/issues/4369.
haskell_cc_import = rule(
  _cc_import_impl,
  attrs = {
    "shared_library": attr.label(),
    "hdrs": attr.label_list(allow_files = [".h"])
  },
)

def _cc_haskell_import(ctx):
  if HaskellPackageInfo in ctx.attr.dep:
    return [DefaultInfo(
      files = set.to_depset(ctx.attr.dep[HaskellPackageInfo].dynamic_libraries)
    )]
  else:
    fail("{0} has to provide HaskellPackageInfo".format(ctx.attr.dep.label.name))

cc_haskell_import = rule(
  _cc_haskell_import,
  attrs = {
    "dep": attr.label(),
  },
)
