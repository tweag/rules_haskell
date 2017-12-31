def cc_headers(ctx):
  # Bring in scope the header files of dependencies, if any.
  hdrs = [
    hdr
    # XXX There's gotta be a better way to test the presence of
    # CcSkylarkApiProvider.
    for dep in ctx.attr.deps if hasattr(dep, "cc")
    for hdr in dep.cc.transitive_headers.to_list()
  ]
  return hdrs, ["-I" + hdr.dirname for hdr in hdrs]
