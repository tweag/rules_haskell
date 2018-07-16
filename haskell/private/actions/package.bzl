"""Action for creating packages and registering them with ghc-pkg"""

load(":private/path_utils.bzl", "target_unique_name")
load(":private/pkg_id.bzl", "pkg_id")
load(":private/set.bzl", "set")
load("@bazel_skylib//:lib.bzl", "paths")

def package(hs, dep_info, interfaces_dir, static_library, dynamic_library, exposed_modules, other_modules, my_pkg_id, static_library_prof, interface_files):
  """Create GHC package using ghc-pkg.

  Args:
    hs: Haskell context.
    interfaces_dir: Directory containing interface files.
    static_library: Static library of the package.
    dynamic_library: Dynamic library of the package.
    static_library_prof: Static library compiled with profiling or None.

  Returns:
    (File, File): GHC package conf file, GHC package cache file
  """
  pkg_db_dir = hs.actions.declare_directory(pkg_id.to_string(my_pkg_id))
  conf_file = hs.actions.declare_file(paths.join(pkg_db_dir.basename, "{0}.conf".format(pkg_id.to_string(my_pkg_id))))
  cache_file = hs.actions.declare_file("package.cache", sibling=conf_file)

  # Create a file from which ghc-pkg will create the actual package from.
  registration_file = hs.actions.declare_file(target_unique_name(hs, "registration-file"))
  registration_file_entries = {
    "name": my_pkg_id.name,
    "version": my_pkg_id.version,
    "id": pkg_id.to_string(my_pkg_id),
    "key": pkg_id.to_string(my_pkg_id),
    "exposed": "True",
    "exposed-modules": " ".join(set.to_list(exposed_modules)),
    "hidden-modules": " ".join(set.to_list(other_modules)),
    "import-dirs": paths.join("${pkgroot}", paths.basename(interfaces_dir)),
    "library-dirs": "${pkgroot}",
    "dynamic-library-dirs": "${pkgroot}",
    "hs-libraries": pkg_id.library_name(hs, my_pkg_id),
    "depends":
      ", ".join(
        # XXX Ideally we would like to specify here prebuilt dependencies
        # too, but we don't know their versions, and package ids without
        # versions will be rejected as unknown.
        set.to_list(dep_info.package_ids)),
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
      depset(interface_files),
      depset([static_library, registration_file, dynamic_library]
             + ([static_library_prof] if static_library_prof != None else [])),
    ]),
    outputs = [conf_file, pkg_db_dir, cache_file],
    env = {
      "GHC_PACKAGE_PATH": package_path,
    },
    mnemonic = "HaskellRegisterPackage",
    executable = hs.tools.ghc_pkg,
    arguments = [
      "register", "--package-db={0}".format(pkg_db_dir.path),
      "-v0",
      "--no-expand-pkgroot",
      registration_file.path
    ]
  )

  return conf_file, cache_file
