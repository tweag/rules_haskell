"""Rule for a repo containing an auxiliary toolchain with binutils."""

def _locate_tool(ctx, tool_name):
  full_path = ctx.which(tool_name)
  if full_path == None:
    fail("Could not locale {0}, make sure it's on PATH".format(tool_name))
  else:
    return "{}".format(full_path)

def _binutils_repo_impl(ctx):
  """TODO
  """

  # We have to do it this way, one by one, because the "substitutions"
  # argument to ctx.template must be a dictionary from string to string.

  ln_location = _locate_tool(ctx, "ln")
  grep_location = _locate_tool(ctx, "grep")

  ctx.template(
    "BUILD",
    Label("//haskell:binutils.BUILD"),
    executable = False,
  )

  ctx.template(
    "binutils.bzl",
    Label("//haskell:binutils.bzl"),
    substitutions = {
      "ln_location": ln_location,
      "grep_location": grep_location,
    },
    executable = False,
  )

binutils_repository = repository_rule(
  _binutils_repo_impl,
  local = True,
)
