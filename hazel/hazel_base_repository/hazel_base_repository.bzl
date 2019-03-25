load("@bazel_tools//tools/cpp:lib_cc_configure.bzl", "get_cpu_value")
load("@ai_formation_hazel//tools:ghc.bzl", "get_executable_name", "get_ghc_workspace")

def _hazel_base_repository_impl(ctx):
    ghc_workspace = get_ghc_workspace(ctx.attr.ghc_workspaces, ctx)
    ghc_label = get_executable_name("{}//:bin/ghc".format(ghc_workspace), ctx)
    ghc = ctx.path(Label(ghc_label))

    cabal2bazel_srcs = [
        "@ai_formation_hazel//hazel_base_repository:cabal2bazel.hs",
        "@ai_formation_hazel//hazel_base_repository:Flatten.hs",
        "@ai_formation_hazel//hazel_base_repository:Skylark.hs",
        "@ai_formation_hazel//third_party/cabal2bazel:src/Google/Google3/Tools/Cabal2Build/Description.hs",
    ]

    for f in cabal2bazel_srcs:
        l = Label(f)
        ctx.symlink(Label(f), l.name)

    ghc_args = [
        "-Wall",
        "-Werror",
        # Only use core packages of GHC, nothing from the the user level:
        "-clear-package-db",
        "-global-package-db",
        "--make",
        "-o",
        "cabal2bazel",
    ]
    if get_cpu_value(ctx) == "darwin":
        # Nixpkgs commit 3513034208a introduces -liconv in NIX_LDFLAGS on
        # Darwin. We don't currently handle NIX_LDFLAGS in any special
        # way, so a hack is to simply do what NIX_LDFLAGS is telling us we
        # should do always when using a toolchain from Nixpkgs.
        # TODO remove this gross hack.
        ghc_args.append("-liconv")
    res = ctx.execute([ghc] + ghc_args + [Label(f).name for f in cabal2bazel_srcs])
    if res.return_code != 0:
        fail("Couldn't build cabal2bazel:\n{}\n{}".format(res.stdout, res.stderr))

    res = ctx.execute([ghc, "--numeric-version"])
    if res.return_code != 0:
        fail("Couldn't get GHC version:\n{}\n{}".format(res.stdout, res.stderr))

    ctx.file(
        "ghc-version",
        res.stdout.split("\n")[0],
        executable = False,
    )

    ctx.file("extra-libs.bzl", """
extra_libs = {}
""".format(
        str(ctx.attr.extra_libs),
    ))

    ctx.file(
        "BUILD",
        content = """exports_files(["cabal2bazel", "cabal2bazel.exe", "ghc-version"])""",
        executable = False,
    )

hazel_base_repository = repository_rule(
    implementation = _hazel_base_repository_impl,
    attrs = {
        "ghc_workspaces": attr.string_dict(mandatory = True),
        "extra_libs": attr.string_dict(mandatory = True),
    },
)

# TODO: don't reload all package names into every repository.
def symlink_and_invoke_hazel(ctx, hazel_base_repo_name, ghc_workspace, package_flags, cabal_path, output):
    cabal2bazel = get_executable_name("cabal2bazel", ctx)
    for f in [cabal2bazel, "ghc-version"]:
        ctx.symlink(Label("@" + hazel_base_repo_name + "//:" + f), f)

    cat_ghc_version = ctx.execute(["cat", "ghc-version"])
  
    if cat_ghc_version.return_code != 0:
      fail("Could not read ghc-version, got return code {}, stderr: {}, stdout: {}".format(
              cat_ghc_version.return_code,
              cat_ghc_version.stderr,
              cat_ghc_version.stdout,
              ))
  
    ghc_version = cat_ghc_version.stdout

    flag_args = []

    for flag in package_flags:
        if package_flags[flag] == "True":
            flag_args += ["-flag-on", flag]
        elif package_flags[flag] == "False":
            flag_args += ["-flag-off", flag]

    res = ctx.execute([
        "./{}".format(cabal2bazel),
        ghc_version,
        cabal_path,
        output,
    ] + flag_args, quiet = False)

    if res.return_code != 0:
        fail("Error running hazel on {}:\n{}\n{}".format(
            cabal_path,
            res.stdout,
            res.stderr,
        ))
    if res.stderr:
        print(res.stderr)
    ctx.file("BUILD", """
load("@ai_formation_hazel//third_party/cabal2bazel:bzl/cabal_package.bzl",
     "cabal_haskell_package",
     "hazel_symlink")
load("@hazel_base_repository//:extra-libs.bzl",
  "extra_libs",
)
load("@{workspace_name}//:package.bzl", "package")
# Make a buildable target for easier debugging of the package.bzl file
hazel_symlink(
  name = "bzl",
  src = "package.bzl",
  out = "package-bzl",
)
cabal_haskell_package(
  package,
  "{ghc_version}",
  "{ghc_workspace}",
  extra_libs,
)
""".format(
        workspace_name = ctx.name,
        ghc_version = ghc_version,
        ghc_workspace = ghc_workspace,
    ))
