"""Package list handling"""

load(":private/path_utils.bzl", "get_dirname", "get_lib_name")
load(":private/set.bzl", "set")

def pkg_info_to_compile_flags(pkg_info, for_plugin = False):
    """Map package info to GHC command-line arguments.

    Args:
      pkg_info: Package info collected by `ghc_info()`.
      for_plugin: Whether the package is a plugin dependency.

    Returns:
      The list of command-line arguments that should be passed to GHC.
    """
    namespace = "plugin-" if for_plugin else ""
    args = [
        # In compile.bzl, we pass this just before all -package-id
        # arguments. Not doing so leads to bizarre compile-time failures.
        # It turns out that equally, not doing so leads to bizarre
        # link-time failures. See
        # https://github.com/tweag/rules_haskell/issues/395.
        "-hide-all-{}packages".format(namespace),
    ]

    if not pkg_info.has_version:
        args.extend([
            # Macro version are disabled for all packages by default
            # and enabled for package with version
            # see https://github.com/tweag/rules_haskell/issues/414
            "-fno-version-macros",
        ])

    for package_id in pkg_info.package_ids:
        args.extend(["-{}package-id".format(namespace), package_id])

    for package_db in pkg_info.package_dbs:
        args.extend(["-package-db", package_db])

    return args

def expose_packages(hs_info, lib_info, use_direct, use_my_pkg_id, custom_package_databases, version):
    """
    Returns the information that is needed by GHC in order to enable haskell
    packages.

    hs_info: is common to all builds
    version: if the rule contains a version, we will export the CPP version macro

    All the other arguments are not understood well:

    lib_info: only used for repl and linter
    use_direct: only used for repl and linter
    use_my_pkg_id: only used for one specific task in compile.bzl
    custom_package_databases: override the package_databases of hs_info, used only by the repl
    """
    has_version = version != None and version != ""

    # Expose all bazel dependencies
    package_ids = []
    for package in hs_info.package_ids:
        # XXX: repl and lint uses this lib_info flags
        # It is set to None in all other usage of this function
        # TODO: find the meaning of this flag
        if lib_info == None or package != lib_info.package_id:
            # XXX: use_my_pkg_id is not None only in compile.bzl
            if (use_my_pkg_id == None) or package != use_my_pkg_id:
                package_ids.append(package)

    # Only include package DBs for deps.
    package_dbs = []
    for cache in hs_info.package_databases.to_list() if custom_package_databases == None else custom_package_databases.to_list():
        package_dbs.append(cache.dirname)

    ghc_info = struct(
        has_version = has_version,
        package_ids = package_ids,
        package_dbs = package_dbs,
    )
    return ghc_info

def write_package_conf(ctx, conf_file, metadata):
    """Write GHC package configuration file.

    See https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/packages.html

    Args:
      ctx: Rule context or Haskell context.
      conf_file: The declared output file.
      metadata: Dictionary from metadata keys to values.
        Sequence or depset values should be passed as such. Scalar values as scalars.
    """
    # Use an Args object to avoid converting depsets to lists and building up large strings.
    package_conf = ctx.actions.args()
    package_conf.set_param_file_format("multiline")

    formats = {
        "name": ("add", {"format": "name: %s"}),
        "id": ("add", {"format": "id: %s"}),
        "key": ("add", {"format": "key: %s"}),
        "version": ("add", {"format": "version: %s"}),
        "exposed": ("add", {"format": "exposed: %s"}),
        "depends": ("add_joined", {"join_with": " ", "format_joined": "depends: %s"}),
        "exposed-modules": ("add_joined", {"join_with": " ", "format_joined": "exposed-modules: %s"}),
        "hidden-modules": ("add_joined", {"join_with": " ", "format_joined": "hidden-modules: %s"}),
        "import-dirs": ("add_joined", {"join_with": " ", "format_joined": "import-dirs: %s", "uniquify": True}),
        "hs-libraries": ("add_joined", {"join_with": " ", "format_joined": "hs-libraries: %s"}),
        "extra-libraries": ("add_joined", {"join_with": " ", "format_joined": "extra-libraries: %s"}),
        "extra-ghci-libraries": ("add_joined", {"join_with": " ", "format_joined": "extra-ghci-libraries: %s"}),
        "library-dirs": ("add_joined", {"join_with": " ", "format_joined": "library-dirs: %s", "uniquify": True}),
        "dynamic-library-dirs": ("add_joined", {"join_with": " ", "format_joined": "dynamic-library-dirs: %s", "uniquify": True}),
        "include-dirs": ("add_joined", {"join_with": " ", "format_joined": "include-dirs: %s", "uniquify": True}),
        "cc-options": ("add_joined", {"join_with": " ", "format_joined": "cc-options: %s"}),
        "ld-options": ("add_joined", {"join_with": " ", "format_each": '"%s"', "format_joined": "ld-options: %s"}),
        # Custom fields that map to GHC package configuration fields.
        "cc-defines": ("add_joined", {"join_with": " ", "format_each": '"-D%s"', "format_joined": "cc-options: %s", "uniquify": True}),
        "cc-quote-includes": ("add_joined", {"join_with": " ", "format_each": '"-iquote%s"', "format_joined": "cc-options: %s", "uniquify": True}),
        "cc-system-includes": ("add_joined", {"join_with": " ", "format_each": '"-isystem%s"', "format_joined": "cc-options: %s", "uniquify": True}),
        "ld-static-libs": ("add_joined", {"join_with": " ", "format_joined": "ld-options: %s"}),
        "ld-dynamic-libs": ("add_joined", {"join_with": " ", "map_each": get_lib_name, "format_joined": "extra-libraries: %s", "uniquify": True}),
        "ld-dynamic-libdirs": ("add_joined", {"join_with": " ", "map_each": get_dirname, "format_joined": "dynamic-library-dirs: %s", "uniquify": True}),
        # NOTE: Add additional fields as needed.
    }

    for (k, v) in metadata.items():
        if not v:
            continue
        (fn, kwargs) = formats[k]
        if fn == "add":
            package_conf.add(v, **kwargs)
        elif fn == "add_joined":
            package_conf.add_joined(v, **kwargs)
        else:
            fail("Internal error: unknown package_conf function")

    ctx.actions.write(conf_file, package_conf)

def ghc_pkg_recache(ctx, ghc_pkg, conf_file):
    """Generate a package.cache file from the given package configuration

    Args:
      ctx: Rule context or Haskell context.
      ghc_pkg: The ghc-pkg tool.
      conf_file: The package configuration file.

    Returns:
      File, the package.cache file.
    """
    cache_file = ctx.actions.declare_file("package.cache", sibling = conf_file)
    ctx.actions.run(
        executable = ghc_pkg,
        arguments = [
            "recache",
            "--package-db={}".format(conf_file.dirname),
            "-v0",
            "--no-expand-pkgroot",
        ],
        mnemonic = "HaskellRegisterPackage",
        progress_message = "HaskellRegisterPackage {}".format(ctx.label),
        outputs = [cache_file],
        inputs = depset(direct = [conf_file]),
    )
    return cache_file
