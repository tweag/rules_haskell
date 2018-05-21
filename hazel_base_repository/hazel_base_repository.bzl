def _hazel_base_repository_impl(ctx):
  ctx.symlink(ctx.attr.ghc, "ghc")

  cabal2bazel_srcs = [
      "@ai_formation_hazel//hazel_base_repository:cabal2bazel.hs",
      "@ai_formation_hazel//hazel_base_repository:Flatten.hs",
      "@ai_formation_hazel//hazel_base_repository:Skylark.hs",
      "@ai_formation_hazel//third_party/cabal2bazel:src/Google/Google3/Tools/Cabal2Build/Description.hs",
  ]

  for f in cabal2bazel_srcs:
    l = Label(f)
    ctx.symlink(Label(f), l.name)

  res = ctx.execute(["./ghc", "-Wall", "-Werror", "--make", "-o", "cabal2bazel"]
                    + [Label(f).name for f in cabal2bazel_srcs])
  if res.return_code != 0:
    fail("Couldn't build cabal2bazel:\n{}\n{}".format(res.stdout,res.stderr))

  res = ctx.execute(["./ghc", "--numeric-version"])
  if res.return_code != 0:
    fail("Couldn't get GHC version:\n{}\n{}".format(res.stdout,res.stderr))

  ctx.file(
      "ghc-version",
      res.stdout.split("\n")[0],
      executable=False)

  ctx.file("packages.bzl", """
prebuilt_dependencies = {}
packages = {}
""".format(str(ctx.attr.prebuilt_dependencies), str(ctx.attr.packages)))

  ctx.file("extra-libs.bzl", """
extra_libs = {}
""".format(str(ctx.attr.extra_libs)))

  ctx.file(
      "BUILD",
      content="""exports_files(["cabal2bazel", "ghc-version"])""",
      executable=False)

hazel_base_repository = repository_rule(
    implementation=_hazel_base_repository_impl,
    attrs={
        "ghc": attr.label(mandatory=True),
        "packages": attr.string_dict(mandatory=True),
        "prebuilt_dependencies": attr.string_dict(mandatory=True),
        "extra_libs": attr.string_dict(mandatory=True),
    })

# TODO: don't reload all package names into every repository.
def symlink_and_invoke_hazel(ctx, hazel_base_repo_name, cabal_path, output):
  for f in ["cabal2bazel", "ghc-version"]:
    ctx.symlink(Label("@" + hazel_base_repo_name + "//:" + f), f)

  ghc_version = ctx.execute(["cat", "ghc-version"]).stdout

  res = ctx.execute(["./cabal2bazel", ghc_version, cabal_path, "package.bzl"])
  if res.return_code != 0:
    fail("Error running hazel on {}:\n{}\n{}".format(
        cabal_path, res.stdout, res.stderr))
  if res.stderr:
    print(res.stderr)
  ctx.file("BUILD", """
load("@ai_formation_hazel//third_party/cabal2bazel:bzl/cabal_package.bzl",
     "cabal_haskell_package",
     "hazel_symlink")
load("@hazel_base_repository//:packages.bzl", "prebuilt_dependencies")
load("@hazel_base_repository//:extra-libs.bzl", "extra_libs")
load("//:package.bzl", "package")
# Make a buildable target for easier debugging of the package.bzl file
hazel_symlink(
  name = "bzl",
  src = "package.bzl",
  out = "package-bzl",
)
cabal_haskell_package(package, prebuilt_dependencies, extra_libs)
""")
