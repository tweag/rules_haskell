load("//hazel_base_repository:hazel_base_repository.bzl",
     "hazel_base_repository",
     "symlink_and_invoke_hazel")

load("@bazel_tools//tools/build_defs/repo:git.bzl",
     "new_git_repository",
)
load("@bazel_tools//tools/build_defs/repo:http.bzl",
     "http_archive",
)

def _cabal_haskell_repository_impl(ctx):
  pkg = "{}-{}".format(ctx.attr.package_name, ctx.attr.package_version)
  url = "https://hackage.haskell.org/package/{}.tar.gz".format(pkg)
  # If the SHA is wrong, the error message is very unhelpful:
  # https://github.com/bazelbuild/bazel/issues/3709
  # As a workaround, we compute it manually if it's not set (and then fail
  # this rule).
  if not ctx.attr.sha256:
    ctx.download(url=url, output="tar")
    res = ctx.execute(["openssl", "sha", "-sha256", "tar"])
    fail("Missing expected attribute \"sha256\" for {}; computed {}".format(pkg, res.stdout + res.stderr))

  ctx.download_and_extract(
      url=url,
      stripPrefix=ctx.attr.package_name + "-" + ctx.attr.package_version,
      sha256=ctx.attr.sha256,
      output="")

  symlink_and_invoke_hazel(
    ctx,
    ctx.attr.hazel_base_repo_name,
    ctx.attr.package_flags,
    ctx.attr.package_name + ".cabal",
    "package.bzl"
  )

_cabal_haskell_repository = repository_rule(
    implementation=_cabal_haskell_repository_impl,
    attrs={
        "package_name": attr.string(mandatory=True),
        "package_version": attr.string(mandatory=True),
        "package_flags": attr.string_dict(mandatory=True),
        "hazel_base_repo_name": attr.string(mandatory=True),
        "sha256": attr.string(mandatory=True),
    })

def _core_library_repository_impl(ctx):
  ctx.file(
    "BUILD",
    executable=False,
    content="""
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_import")
haskell_import(
    name = "{pkg}",
    package = "{pkg}",
    visibility = ["//visibility:public"],
)
""".format(pkg=ctx.attr.package))

_core_library_repository = repository_rule(
    implementation=_core_library_repository_impl,
    attrs={
        "package": attr.string(mandatory=True),
    })

def _all_hazel_packages_impl(ctx):
  ctx.file("BUILD", """
filegroup(
    name = "all-package-files",
    srcs = [{}],
)
           """.format(",".join(["\"{}\"".format(f) for f in ctx.attr.files])),
          executable=False)

_all_hazel_packages = repository_rule(
    implementation=_all_hazel_packages_impl,
    attrs={
        "files": attr.label_list(mandatory=True),
    })

def _fixup_package_name(package_name):
  """Fixup package name by replacing dashes with underscores to get a valid
  workspace name from it.

  Args:
    package_name: string: Package name.

  Returns:
    string: fixed package name.
  """
  return package_name.replace("-", "_")

def hazel_repositories(
  core_packages,
  packages,
  extra_flags={},
  extra_libs={},
  extra_libs_hdrs={},
  extra_libs_strip_include_prefix={},
  exclude_packages=[]):
  """Generates external dependencies for a set of Haskell packages.

  This macro should be invoked in the WORKSPACE.  It generates a set of
  external dependencies corresponding to the given packages:
  - @hazel_base_repository: The compiled "hazel" Haskell binary, along with
    support files.
  - @haskell_{package}: A build of the given Cabal package, one per entry
    of the "packages" argument.  (Note that Bazel only builds these
    on-demand when needed by other rules.)  This repository automatically
    downloads the package's Cabal distribution from Hackage and parses the
    .cabal file to generate BUILD rules.
  - @all_hazel_packages: A repository depending on each package.  Useful for
    measuring our coverage of the full package set.

  Args:
    core_packages: A dict mapping Haskell package names to version
      numbers.  These packages are assumed to be provided by GHC.
    packages: A dict mapping strings to structs, where each struct has two fields:
      - version: A version string
      - sha256: A hex-encoded SHA of the Cabal distribution (*.tar.gz).
    extra_flags: A dict mapping package names to cabal flags.
    exclude_packages: names of packages to exclude.
    extra_libs: A dictionary that maps from name of extra libraries to Bazel
      targets that provide the shared library.
    extra_libs_hdrs: Similar to extra_libs, but provides header files.
    extra_libs_strip_include_prefix: Similar to extra_libs, but allows to
      get include prefix to strip.
  """
  hazel_base_repo_name = "hazel_base_repository"

  pkgs = {n: packages[n] for n in packages if n not in exclude_packages}

  hazel_base_repository(
      name = hazel_base_repo_name,
      # TODO: don't hard-code this in
      ghc="@ghc//:bin/ghc",
      extra_libs = extra_libs,
      extra_libs_hdrs = extra_libs_hdrs,
      extra_libs_strip_include_prefix = extra_libs_strip_include_prefix,
  )
  for p in pkgs:

    flags = {}

    if hasattr(pkgs[p], "flags"):
      items = pkgs[p].flags
      # NOTE We have to convert booleans to strings in order to pass them as
      # attributes of the _cabal_haskell_repository rule because there is no
      # attribute type for dictionary of booleans at the moment.
      flags = {flag: str(items[flag]) for flag in items}

    if p in extra_flags:
      items = extra_flags[p]
      flags.update({flag: str(items[flag]) for flag in items})

    _cabal_haskell_repository(
        name = "haskell_" + _fixup_package_name(p),
        package_name = p,
        package_version = pkgs[p].version,
        package_flags = flags,
        sha256 = pkgs[p].sha256 if hasattr(pkgs[p], "sha256") else None,
        hazel_base_repo_name = hazel_base_repo_name,
    )

  for p in core_packages:
    _core_library_repository(
        name = "haskell_" + _fixup_package_name(p),
        package = p,
    )

  _all_hazel_packages(
      name = "all_hazel_packages",
      files = ["@haskell_{}//:files".format(_fixup_package_name(p)) for p in pkgs])

def hazel_library(name):
  """Returns the label of the haskell_library rule for the given package."""
  return "@haskell_{}//:{}".format(_fixup_package_name(name), name)

def hazel_custom_package_hackage(
    package_name,
    version,
    sha256=None):
  """Generate a repo for a Haskell package fetched from Hackage.

  Args:
    package_name: string, package name.
    version: string, package version.
    sha256: string, SHA256 hash of archive.
  """
  package_id = package_name + "-" + version
  url = "https://hackage.haskell.org/package/{0}/{1}.tar.gz".format(
    package_id,
    package_id,
  )
  fixed_package_name = _fixup_package_name(package_name)
  http_archive(
    name = "haskell_{0}".format(fixed_package_name),
    build_file = "//third_party/haskell:BUILD.{0}".format(fixed_package_name),
    sha256 = sha256,
    strip_prefix = package_id,
    urls = [url],
  )

def hazel_custom_package_github(
    package_name,
    github_user,
    github_repo,
    repo_sha,
    strip_prefix=None,
    archive_sha256=None,
    clone_via_ssh=False):
  """Generate a repo for a Haskell package coming from a GitHub repo.

  Args:
    package_name: string, package name.
    github_user: string, GitHub user.
    github_repo: string, repo name under `github_user` account.
    repo_sha: SHA1 of commit in the repo.
    strip_prefix: strip this path prefix from directory repo, useful when a
                  repo contains several packages.
    archive_sha256: hash of the actual archive to download.
    clone_via_ssh: whether to clone the repo using SSH (useful for private
                   repos).
  """

  fixed_package_name = _fixup_package_name(package_name)
  build_file = "//third_party/haskell:BUILD.{0}".format(fixed_package_name)
  url = "https://github.com/{0}/{1}".format(github_user, github_repo)
  ssh_url = "git@github.com:{0}/{1}".format(github_user, github_repo)

  new_git_repository(
    name = "haskell_{0}".format(fixed_package_name),
    remote = ssh_url if clone_via_ssh else url,
    build_file = build_file,
    commit = repo_sha,
    strip_prefix = strip_prefix,
  )
