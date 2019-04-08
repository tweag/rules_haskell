load(
    "//hazel_base_repository:hazel_base_repository.bzl",
    "hazel_base_repository",
    "symlink_and_invoke_hazel",
)
load(
    "@bazel_tools//tools/build_defs/repo:git.bzl",
    "new_git_repository",
)
load(
    "@bazel_tools//tools/build_defs/repo:http.bzl",
    "http_archive",
)
load("//tools:ghc.bzl", "default_ghc_workspaces", "get_ghc_workspace")
load("//tools:mangling.bzl", "hazel_binary", "hazel_library", "hazel_workspace")

def _cabal_haskell_repository_impl(ctx):
    ghc_workspace = get_ghc_workspace(ctx.attr.ghc_workspaces, ctx)

    ctx.download_and_extract(**ctx.attr.download_options)

    symlink_and_invoke_hazel(
        ctx,
        ctx.attr.hazel_base_repo_name,
        ghc_workspace,
        ctx.attr.package_flags,
        ctx.attr.package_name + ".cabal",
        "package.bzl",
    )

_cabal_haskell_repository = repository_rule(
    implementation = _cabal_haskell_repository_impl,
    attrs = {
        "package_name": attr.string(mandatory = True),
        "package_flags": attr.string_dict(mandatory = True),
        "hazel_base_repo_name": attr.string(mandatory = True),
        "download_options": attr.string_dict(mandatory = True),
        "ghc_workspaces": attr.string_dict(mandatory = True),
    },
)

def _core_library_repository_impl(ctx):
    ctx.file(
        "BUILD",
        executable = False,
        content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_import")
haskell_import(
    name = "{pkg}",
    package = "{pkg}",
    visibility = ["//visibility:public"],
)
# Cabal packages can depend on other Cabal package's cbits, for example for
# CPP includes. To enable uniform handling we define a `-cbits` target for
# every Hazel Haskell target. In case of core_libraries this is just a dummy.
cc_import(
    name = "{pkg}-cbits",
    visibility = ["//visibility:public"],
)
""".format(pkg = ctx.attr.package),
    )

_core_library_repository = repository_rule(
    implementation = _core_library_repository_impl,
    attrs = {
        "package": attr.string(mandatory = True),
    },
)

def _all_hazel_packages_impl(ctx):
    all_packages_filegroup = """
filegroup(
    name = "all-package-files",
    srcs = [{}],
)
  """.format(",".join(["\"@{}//:bzl\"".format(hazel_workspace(p)) for p in ctx.attr.packages]))
    one_package_template = """
filegroup(
    name = "haskell_{package_name}",
    srcs = ["@{workspace_name}//:bzl"],
)
  """
    package_filegroups = [
        one_package_template.format(
            package_name = p,
            workspace_name = hazel_workspace(p),
        )
        for p in ctx.attr.packages
    ]
    ctx.file("BUILD", "\n".join([all_packages_filegroup] + package_filegroups), executable = False)

_all_hazel_packages = repository_rule(
    implementation = _all_hazel_packages_impl,
    attrs = {
        "packages": attr.string_list(mandatory = True),
    },
)

def hazel_repositories(
        core_packages,
        packages,
        extra_flags = {},
        extra_libs = {},
        exclude_packages = [],
        ghc_workspaces = default_ghc_workspaces):
    """Generates external dependencies for a set of Haskell packages.

    This macro should be invoked in the WORKSPACE.  It generates a set of
    external dependencies corresponding to the given packages:
    - @hazel_base_repository: The compiled "hazel" Haskell binary, along with
      support files.
    - @haskell_{package}_{hash}: A build of the given Cabal package, one per entry
      of the "packages" argument.  (Note that Bazel only builds these
      on-demand when needed by other rules.)  This repository automatically
      downloads the package's Cabal distribution from Hackage and parses the
      .cabal file to generate BUILD rules.
    - @all_hazel_packages: A repository depending on each package.  Useful for
      measuring our coverage of the full package set.

    Args:
      core_packages: A dict mapping Haskell package names to version
        numbers.  These packages are assumed to be provided by GHC.
      packages: A dict mapping strings to structs, where each struct specifies
      either the package version (for Hackage download) or a URL. In both cases
      the sha256 of the archive must also be provided. When specifying a URL
      download_and_extract options may also be provided: output, type,
      stripPrefix. The following fields may be set:
        - version: A version string.
        - sha256: A hex-encoded SHA of the Cabal distribution (*.tar.gz).
        - url: Where to download the package tarball from (if no 'version' is
          specified).
        - output, type, stripPrefix: Only when "url" is specified. See the
          download_and_extract documentation.
      extra_flags: A dict mapping package names to cabal flags.
      exclude_packages: names of packages to exclude.
      extra_libs: A dictionary that maps from name of extra libraries to Bazel
        targets that provide the shared library and headers as a cc_library.
      ghc_workspaces: Dictionary mapping OS names to GHC workspaces.
        Default: Linux/MacOS: "@ghc", Windows: "@ghc_windows".
        Dictionary keys correspond to CPU values as returned by
        `get_cpu_value` from `@bazel_tools//tools/cpp:lib_cc_configure.bzl`.
    """
    hazel_base_repo_name = "hazel_base_repository"

    pkgs = {n: packages[n] for n in packages if n not in exclude_packages}

    hazel_base_repository(
        name = hazel_base_repo_name,
        ghc_workspaces = ghc_workspaces,
        extra_libs = extra_libs,
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

        package_name = p

        # download_options should have the following mandatory
        # field for download_and_extract:
        #   * url
        # Moreover the following field is optional for download_and_extra but, for
        # reproducibility's sake, we make it mandatory:
        #   * sha256
        # In the Hackage case, the URL is set to the tarball's on Hackage.
        # In the case of the user specifying "url", we forward these other fields:
        #   * output
        #   * type
        #   * stripPrefix
        download_options = {}

        sha256 = pkgs[p].sha256 if hasattr(pkgs[p], "sha256") else fail("{} is missing attribute sha256".format(package_name))
        download_options.update({"sha256": sha256})

        # If "version" is present, the package will be fetched from Hackage.
        if hasattr(pkgs[p], "version"):
            package_version = pkgs[p].version
            pkg = "{}-{}".format(package_name, package_version)

            url = "https://hackage.haskell.org/package/{}.tar.gz".format(pkg)
            download_options.update({"url": url})

            stripPrefix = package_name + "-" + package_version
            download_options.update({"stripPrefix": stripPrefix})

            # If "url" is present, the package will be fetched from the given URL.
        elif hasattr(pkgs[p], "url"):
            download_options.update({"url": pkgs[p].url})
            download_options.update({"output": pkgs[p].output} if hasattr(pkgs[p], "output") else {})
            download_options.update({"type": pkgs[p].type} if hasattr(pkgs[p], "type") else {})
            download_options.update({"stripPrefix": pkgs[p].stripPrefix} if hasattr(pkgs[p], "stripPrefix") else {})

        else:
            fail("Package {} should have either 'url' or 'version'").format(pkg)

        _cabal_haskell_repository(
            name = hazel_workspace(p),
            package_name = p,
            package_flags = flags,
            hazel_base_repo_name = hazel_base_repo_name,
            download_options = download_options,
            ghc_workspaces = ghc_workspaces,
        )

    for p in core_packages:
        _core_library_repository(
            name = hazel_workspace(p),
            package = p,
        )

    _all_hazel_packages(
        name = "all_hazel_packages",
        packages = [p for p in pkgs],
    )

def hazel_custom_package_hackage(
        package_name,
        version,
        sha256 = None,
        build_file = None,
        build_file_content = None):
    """Generate a repo for a Haskell package fetched from Hackage.

    Args:
      package_name: string, package name.
      version: string, package version.
      sha256: string, SHA256 hash of archive.
      build_file: string,
        the file to use as the BUILD file for this package.
        Defaults to //third_party/haskel:BUILD.<package_name> if
        neither build_file nor build_file_content are specified.
        This attribute is a label relative to the main workspace.
        build_file and build_file_content are mutually exclusive.
      build_file_content: string,
        the content for the BUILD file for this repository.
        Will fall back to build_file if not specified.
        build_file and build_file_content are mutually exclusive.
    """
    package_id = package_name + "-" + version
    url = "https://hackage.haskell.org/package/{0}/{1}.tar.gz".format(
        package_id,
        package_id,
    )
    if not build_file and not build_file_content:
        build_file = "//third_party/haskell:BUILD.{0}".format(package_name)
    http_archive(
        name = hazel_workspace(package_name),
        build_file = build_file,
        build_file_content = build_file_content,
        sha256 = sha256,
        strip_prefix = package_id,
        urls = [url],
    )

def hazel_custom_package_github(
        package_name,
        github_user,
        github_repo,
        repo_sha,
        clone_via_ssh = False,
        **kwargs):
    """Generate a repo for a Haskell package coming from a GitHub repo. This is
        mostly a wrapper around `new_git_repository`. All arguments not listed
        below will be passed directly to `new_git_repository`.

        NOTE: `name`, `remote` and `commit` should _not_ be specified, those
        are inferred by `hazel_custom_package_github`.

    Args:
      package_name: string, package name.
      github_user: string, GitHub user.
      github_repo: string, repo name under `github_user` account.
      repo_sha: SHA1 of commit in the repo.
      clone_via_ssh: whether to clone the repo using SSH (useful for private
                     repos).
      build_file: string,
        the file to use as the BUILD file for this package.
        Defaults to //third_party/haskel:BUILD.<package_name> if
        neither build_file nor build_file_content are specified.
        This attribute is a label relative to the main workspace.
        build_file and build_file_content are mutually exclusive.
      build_file_content: string,
        the content for the BUILD file for this repository.
        Will fall back to build_file if not specified.
        build_file and build_file_content are mutually exclusive.
    """

    build_file = kwargs.get("build_file")
    build_file_content = kwargs.get("build_file_content")

    err_str = "hazel_custom_package_github: error: {}"

    if not build_file and not build_file_content:
        build_file = "//third_party/haskell:BUILD.{0}".format(package_name)
    url = "https://github.com/{0}/{1}".format(github_user, github_repo)
    ssh_url = "git@github.com:{0}/{1}".format(github_user, github_repo)

    new_kwargs = {}

    for k, v in kwargs.items():
        new_kwargs[k] = v

    new_kwargs["name"] = hazel_workspace(package_name) if not "name" in new_kwargs else fail(err_str.format("Do not specify 'name'"))

    new_kwargs["remote"] = ssh_url if clone_via_ssh else url if not "remote" in new_kwargs else fail(err_str.format("Do not specify 'remote'"))

    if build_file and build_file_content:
        fail(err_str.format("Please specify either build_file or build_file_content, not both"))
    elif build_file:
        new_kwargs["build_file"] = build_file
    else:
        new_kwargs["build_file_content"] = build_file_content

    new_kwargs["commit"] = repo_sha if not "commit" in new_kwargs else fail(err_str.format("Do not specify 'commit'"))

    new_git_repository(**new_kwargs)

def hazel_extra_packages(pkgs, extra_pkgs):
    """Override or add extra packages to the snapshot.

    Args:
      pkgs: A dict mapping strings to structs as expected by hazel_repositories.
      extra_pkgs: A dict mapping strings to dicts.
        The keys represent the name of the package to override or add.
        The dicts represent the fields of the corresponding package struct as
        expected by hazel_repositories. See there for details.
    """
    pkgs = dict(pkgs)
    pkgs.update({k: struct(**v) for (k, v) in extra_pkgs.items()})
    return pkgs
