"""Repository rule to create an auxiliary toolchain with binutils."""

def _binutils_repo_impl(ctx):

  tools = []

  for t in ctx.attr.provide_tools:
    full_path = ctx.which(t)
    if full_path == None:
      fail("Could not locale {0}, make sure it's on PATH".format(t))
    else:
      tools.append(full_path)

  ctx.template(
    "BUILD",
    Label("//haskell:binutils.BUILD"),
    substitutions = {
      "tools": tools,
    },
    executable = False,
  )

  return

binutils_toolchain_repository = repository_rule(
  implementation = _binutils_repo_impl,
  local = True,
  attrs = {
    "provide_tools": attr.string_list(
      doc = "The collection of executable names to locate and provide."
    ),
  }
)
