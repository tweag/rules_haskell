"""Interop with cc_* rules

These rules are temporary and will be deprecated in the future.
"""

load(":providers.bzl",
     "HaskellPackageInfo",
     "CcSkylarkApiProviderHacked",
)

load(":set.bzl", "set")

def cc_headers(ctx):
  """Bring in scope the header files of dependencies, if any.

  *Internal function - do not use.*
  """
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
    "shared_library": attr.label(
      # NOTE We do not list all extensions here because .so libraries may
      # have numeric suffixes like foo.so.1.2.3, and if they also have
      # SONAME with numeric suffix, matching file must be provided, so this
      # attributes must accept libraries with almost arbitrary extensions.
      # It would be easier if Skylark supported regexps.
      allow_files = True,
      doc = """A single precompiled shared library.

Bazel ensures it is available to the binary that depends on it
during runtime.
""",
    ),
    "hdrs": attr.label_list(
      allow_files = [".h"],
      doc = """

The list of header files published by this precompiled library to be
directly included by sources in dependent rules.
""",
    )
  },
)
"""Imports a prebuilt shared library.

Use this to make `.so`, `.dll`, `.dylib` files residing in external
[external repositories][bazel-ext-repos] available to Haskell rules.

*This rule is temporary replacement for [cc_import][cc_import] and
will be deprecated in the future.*

Example:
  ```bzl
  haskell_cc_import(name = "zlib", shared_library = "@zlib//:lib")

  haskell_binary(
    name = "crc32sum",
    srcs = ["Main.hs"],
    deps = [":zlib"],
    prebuilt_dependencies = ["base"],
  )
  ```

[bazel-ext-repos]: https://docs.bazel.build/versions/master/external.html
[cc_import]: https://docs.bazel.build/versions/master/be/c-cpp.html#cc_import
"""

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
    "dep": attr.label(
      doc = "Target providing a `HaskellPackageInfo`, such as `haskell_library`."
    ),
  },
)
"""Exports a Haskell library as a CC library.

Given a [haskell_library](#haskell_library) input, outputs the shared
object file produced as well as the object files it depends on
directly and transitively. This is very useful if you want to link in
a Haskell shared library from `cc_library`.

There is a caveat: this will not provide any shared libraries that
aren't explicitly given to it. This means that if you're using
`prebuilt_dependencies` and relying on GHC to provide those objects,
they will not be present here. You will have to provide those
separately to your `cc_library`. If you're getting
`prebuilt_dependencies` from your toolchain, you will likely want to
extract those and pass them in as well.

*This rule is temporary and only needed until the Bazel C/C++
"sandwich" (see [bazelbuild/bazel#2163][bazel-cpp-sandwich]) is
implemented. This rule will be deprecated in the future.*

Example:
  ```bzl
  haskell_library(
    name = "my-lib",
    ...
  )

  cc_haskell_import(
    name = "my-lib-objects",
    dep = ":my-lib",
  )

  cc_library(
    name = "my-cc",
    srcs = ["main.c", ":my-lib-objects"],
  )
  ```

[bazel-cpp-sandwich]: https://github.com/bazelbuild/bazel/issues/2163

"""
