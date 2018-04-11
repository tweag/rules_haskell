
hazel_srcs = ["Main.hs", "Build.hs", "Skylark.hs", "Flatten.hs"]

def _hazel_base_repository_impl(ctx):
  ctx.symlink(ctx.attr.ghc, "ghc")

  for f in hazel_srcs:
    ctx.symlink(Label("@ai_formation_hazel//hazel_base_repository:" + f), f)

  res = ctx.execute(["./ghc", "--make", "-o", "hazel"] + hazel_srcs)
  if res.return_code != 0:
    fail("Couldn't build hazel:\n{}\n{}".format(res.stdout,res.stderr))

  res = ctx.execute(["./ghc", "--numeric-version"])
  if res.return_code != 0:
    fail("Couldn't build hazel:\n{}\n{}".format(res.stdout,res.stderr))

  ctx.file(
      "ghc-version",
      res.stdout.split("\n")[0],
      executable=False)

  ctx.file(
      "BUILD",
      content="""exports_files(["hazel", "ghc-version", "all-packages"])""",
      executable=False)

  ctx.file(
      "all-packages",
      "\n".join(ctx.attr.versions))
  ctx.file(
      "prebuilt-dependencies",
      "\n".join(ctx.attr.prebuilt_dependencies))


hazel_base_repository = repository_rule(
    implementation=_hazel_base_repository_impl,
    attrs={
        "ghc": attr.label(mandatory=True),
        # TODO: rename "packages"
        "versions": attr.string_list(mandatory=True),
        "prebuilt_dependencies": attr.string_list(mandatory=True),
    })

def symlink_and_invoke_hazel(ctx, hazel_base_repo_name, cabal_path, output):
  for f in ["ghc-version", "all-packages", "prebuilt-dependencies", "hazel"]:
    ctx.symlink(Label("@" + hazel_base_repo_name + "//:" + f), f)

  ghc_version = ctx.execute(["cat", "ghc-version"]).stdout

  res = ctx.execute(["./hazel", ghc_version, "all-packages", "prebuilt-dependencies",
                      cabal_path, output])
  if res.return_code != 0:
    fail("Error running hazel on {}:\n{}\n{}".format(
        cabal_path, res.stdout, res.stderr))
