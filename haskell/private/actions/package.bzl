"""Action for creating packages and registering them with ghc-pkg"""

load(":private/providers.bzl",
  "HaskellBuildInfo",
  "HaskellLibraryInfo",
)
load(":private/path_utils.bzl", "target_unique_name")
load(":private/set.bzl", "set")
load(":private/tools.bzl", "tools")
load("@bazel_skylib//:lib.bzl", "paths")

def _zencode(s):
  """Zero-escape a given string.

  Args:
    s: inputs string.

  Returns:
    string: zero-escaped string.
  """
  return s.replace("Z", "ZZ").replace("_", "ZU").replace("/", "ZS")

def get_pkg_name(ctx):
  """Get package name.

  The name is not required to be unique/injective; however it must be a valid
  GHC package name (i.e., no underscores).  This encoding does not aim to
  change as little as possible since it is used for display and also for the
  "PackageImports" extension.

  Args:
    ctx: Rule context

  Returns:
    string: GHC package name to use.
  """
  return ctx.attr.name.replace("_", "-")

def get_pkg_id(ctx):
  """Get package identifier of the form `name-version`.

  The identifier is required to be unique for each Haskell rule.
  It includes the Bazel package and the name of the this component.
  We can't use just the latter because then two components with
  the same names in different packages would clash.

  Args:
    ctx: Rule context

  Returns:
    string: GHC package ID to use.
  """
  return "{0}-{1}".format(
    _zencode(paths.join(
        ctx.label.workspace_root,
        ctx.label.package,
        ctx.attr.name)),
    ctx.attr.version)

def get_library_name(ctx):
  """Get core library name for this package. This is "HS" followed by package ID.

  See https://ghc.haskell.org/trac/ghc/ticket/9625 .

  Args:
    ctx: Rule context.

  Returns:
    string: Library name suitable for GHC package entry.
  """
  return "HS{0}".format(get_pkg_id(ctx))

def create_ghc_package(ctx, dep_info, interfaces_dir, static_library, dynamic_library, exposed_modules, other_modules):
  """Create GHC package using ghc-pkg.

  Args:
    ctx: Rule context.
    interfaces_dir: Directory containing interface files.
    static_library: Static library of the package.
    dynamic_library: Dynamic library of the package.

  Returns:
    (File, File): GHC package conf file, GHC package cache file
  """
  pkg_db_dir = ctx.actions.declare_directory(get_pkg_id(ctx))
  conf_file = ctx.actions.declare_file(paths.join(pkg_db_dir.basename, "{0}.conf".format(get_pkg_id(ctx))))
  cache_file = ctx.actions.declare_file("package.cache", sibling=conf_file)

  # Create a file from which ghc-pkg will create the actual package from.
  registration_file = ctx.actions.declare_file(target_unique_name(ctx, "registration-file"))
  registration_file_entries = {
    "name": get_pkg_name(ctx),
    "version": ctx.attr.version,
    "id": get_pkg_id(ctx),
    "key": get_pkg_id(ctx),
    "exposed": "True",
    "exposed-modules": " ".join(set.to_list(exposed_modules)),
    "hidden-modules": " ".join(set.to_list(other_modules)),
    "import-dirs": paths.join("${pkgroot}", interfaces_dir.basename),
    "library-dirs": "${pkgroot}",
    "dynamic-library-dirs": "${pkgroot}",
    "hs-libraries": get_library_name(ctx),
    "depends":
      ", ".join([ d[HaskellLibraryInfo].package_id for d in
                  ctx.attr.deps if HaskellLibraryInfo in d])
  }
  ctx.actions.write(
    output=registration_file,
    content="\n".join(['{0}: {1}'.format(k, v)
                       for k, v in registration_file_entries.items()])
  )

  # Make the call to ghc-pkg and use the registration file
  package_path = ":".join([c.dirname for c in set.to_list(dep_info.package_confs)])
  ctx.actions.run(
    inputs = depset(transitive = [
      set.to_depset(dep_info.package_confs),
      set.to_depset(dep_info.package_caches),
      depset([static_library, interfaces_dir, registration_file, dynamic_library]),
    ]),
    outputs = [pkg_db_dir, conf_file, cache_file],
    env = {
      "GHC_PACKAGE_PATH": package_path,
    },
    executable = tools(ctx).ghc_pkg,
    arguments = [
      "register", "--package-db={0}".format(pkg_db_dir.path),
      "-v0",
      "--no-expand-pkgroot",
      registration_file.path
    ]
  )

  return conf_file, cache_file
