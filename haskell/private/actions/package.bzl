"""Action for creating packages and registering them with ghc-pkg"""

load(":private/path_utils.bzl", "target_unique_name")
load(":private/set.bzl", "set")
load("@bazel_skylib//:lib.bzl", "paths")

def _zencode(s):
  """Zero-escape a given string.

  Args:
    s: inputs string.

  Returns:
    string: zero-escaped string.
  """
  return s.replace("Z", "ZZ").replace("_", "ZU").replace("/", "ZS")

def get_pkg_name(hs):
  """Get package name.

  The name is not required to be unique/injective; however it must be a valid
  GHC package name (i.e., no underscores).  This encoding does not aim to
  change as little as possible since it is used for display and also for the
  "PackageImports" extension.

  Args:
    hs: Haskell context.

  Returns:
    string: GHC package name to use.
  """
  return hs.name.replace("_", "-")

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

def get_library_name(pkg_id):
  """Get core library name for this package. This is "HS" followed by package ID.

  See https://ghc.haskell.org/trac/ghc/ticket/9625 .

  Args:
    pkg_id: Package ID string.

  Returns:
    string: Library name suitable for GHC package entry.
  """
  return "HS{0}".format(pkg_id)

def package(hs, dep_info, interfaces_dir, static_library, dynamic_library, exposed_modules, other_modules, pkg_id, version, pkg_deps):
  """Create GHC package using ghc-pkg.

  Args:
    hs: Haskell context.
    interfaces_dir: Directory containing interface files.
    static_library: Static library of the package.
    dynamic_library: Dynamic library of the package.

  Returns:
    (File, File): GHC package conf file, GHC package cache file
  """
  pkg_db_dir = hs.actions.declare_directory(pkg_id)
  conf_file = hs.actions.declare_file(paths.join(pkg_db_dir.basename, "{0}.conf".format(pkg_id)))
  cache_file = hs.actions.declare_file("package.cache", sibling=conf_file)

  # Create a file from which ghc-pkg will create the actual package from.
  registration_file = hs.actions.declare_file(target_unique_name(hs, "registration-file"))
  registration_file_entries = {
    "name": get_pkg_name(hs),
    "version": version,
    "id": pkg_id,
    "key": pkg_id,
    "exposed": "True",
    "exposed-modules": " ".join(set.to_list(exposed_modules)),
    "hidden-modules": " ".join(set.to_list(other_modules)),
    "import-dirs": paths.join("${pkgroot}", interfaces_dir.basename),
    "library-dirs": "${pkgroot}",
    "dynamic-library-dirs": "${pkgroot}",
    "hs-libraries": get_library_name(pkg_id),
    "depends":
      ", ".join([d.package_id for d in pkg_deps]),
  }
  hs.actions.write(
    output=registration_file,
    content="\n".join(['{0}: {1}'.format(k, v)
                       for k, v in registration_file_entries.items()])
  )

  # Make the call to ghc-pkg and use the registration file
  package_path = ":".join([c.dirname for c in set.to_list(dep_info.package_confs)])
  hs.actions.run(
    inputs = depset(transitive = [
      set.to_depset(dep_info.package_confs),
      set.to_depset(dep_info.package_caches),
      depset([static_library, interfaces_dir, registration_file, dynamic_library]),
    ]),
    outputs = [pkg_db_dir, conf_file, cache_file],
    env = {
      "GHC_PACKAGE_PATH": package_path,
    },
    executable = hs.tools.ghc_pkg,
    arguments = [
      "register", "--package-db={0}".format(pkg_db_dir.path),
      "-v0",
      "--no-expand-pkgroot",
      registration_file.path
    ]
  )

  return conf_file, cache_file
