"""Package list handling"""

load(":private/path_utils.bzl", "target_unique_name", "truly_relativize")

def pkg_info_to_compile_flags(hs, pkg_info, plugin_pkg_info = None, prefix = ""):
    """Map package info to GHC command-line arguments.

    Passes flags through a GHC environment file where applicable to keep the
    overall command-line length shorter.

    Args:
      hs: Haskell context.
      pkg_info: Package info collected by `expose_packages`.
      plugin_pkg_info: (optional) Plugin package info collected by `expose_packages`.
      prefix: (optional) Distinguishing prefix for the generated package env file.

    Returns:
      (extra_inputs, args)
        extra_inputs: depset of additional input files to GHC.
        args: The list of command-line arguments that should be passed to GHC.
    """
    args = [
        # In compile.bzl, we pass this just before all -package-id
        # arguments. Not doing so leads to bizarre compile-time failures.
        # It turns out that equally, not doing so leads to bizarre
        # link-time failures. See
        # https://github.com/tweag/rules_haskell/issues/395.
        "-hide-all-packages",
    ]

    if not pkg_info.has_version:
        args.extend([
            # Macro version are disabled for all packages by default
            # and enabled for package with version
            # see https://github.com/tweag/rules_haskell/issues/414
            "-fno-version-macros",
        ])

    # Use package environment file for regular package dependencies.
    env_file = hs.actions.declare_file(
        target_unique_name(hs, "{}package_env".format(prefix)),
    )
    args.extend(["-package-env", env_file.path])
    env_args = hs.actions.args()
    for package_id in pkg_info.package_ids:
        env_args.add("package-id {}".format(package_id))
    for package_db in pkg_info.package_databases:
        # paths in package environment files are relative to the file.
        package_db = truly_relativize(package_db, env_file.dirname)
        env_args.add("package-db {}".format(package_db))
    env_args.set_param_file_format("multiline")
    hs.actions.write(env_file, env_args)

    # GHC package environment files don't support plugin flags.
    if plugin_pkg_info:
        args.append("-hide-all-plugin-packages")
        for package_id in plugin_pkg_info.package_ids:
            args.extend(["-plugin-package-id", package_id])
        for package_db in plugin_pkg_info.package_databases:
            args.extend(["-package-db", package_db])

    return (depset([env_file]), args)

def expose_packages(package_ids, package_databases, version):
    """
    Returns the information that is needed by GHC in order to enable haskell
    packages.

    hs_info: is common to all builds
    version: if the rule contains a version, we will export the CPP version macro

    All the other arguments are not understood well:

    my_pkg_id: the pkg_id if we're compiling a library
    custom_package_databases: override the package_databases of hs_info, used only by the repl
    """
    has_version = version != None and version != ""
    ghc_info = struct(
        has_version = has_version,
        package_ids = package_ids,
        package_databases = [cache.dirname for cache in package_databases.to_list()],
    )
    return ghc_info

def write_package_conf(hs, conf_file, metadata):
    """Write GHC package configuration file.

    See https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/packages.html

    Args:
      hs: Haskell context.
      conf_file: The declared output file.
      metadata: Dictionary from package configuration fields to their values.
    """

    # Use an Args object to avoid building up large strings in Starlark.
    package_conf = hs.actions.args()
    package_conf.set_param_file_format("multiline")

    formatters = {
        "name": ("add", {}),
        "id": ("add", {}),
        "key": ("add", {}),
        "version": ("add", {}),
        "exposed": ("add", {}),
        "depends": ("add_joined", {"join_with": ", "}),
        "exposed-modules": ("add_joined", {"join_with": " "}),
        "hidden-modules": ("add_joined", {"join_with": " "}),
        "import-dirs": ("add_joined", {"join_with": " "}),
        "hs-libraries": ("add_joined", {"join_with": " "}),
        "extra-libraries": ("add_joined", {"join_with": " "}),
        "extra-ghci-libraries": ("add_joined", {"join_with": " "}),
        "library-dirs": ("add_joined", {"join_with": " "}),
        "dynamic-library-dirs": ("add_joined", {"join_with": " "}),
        "ld-options": ("add_joined", {"join_with": " ", "format_each": '"%s"'}),
    }

    for (k, v) in metadata.items():
        if not v:
            continue
        formatter = formatters.get(k)
        if not formatter:
            fail("Unknown package configuration field '{}'.".format(k))
        (method, kwargs) = formatter
        linefmt = "{}: %s".format(k)
        if method == "add":
            package_conf.add(v, format = linefmt, **kwargs)
        elif method == "add_joined":
            package_conf.add_joined(v, format_joined = linefmt, **kwargs)
        else:
            fail("Unknown Args method '{}'.".format(method))

    hs.actions.write(conf_file, package_conf)

def ghc_pkg_recache(hs, posix, conf_file):
    """Run ghc-pkg recache on the given package configuration file.

    Note, this will generate the file package.cache in the same directory as
    conf_file. Calling this function on two different package configuration
    files in the same directory is an error as it will generate conflicting
    actions.

    Args:
      hs: Haskell context.
      conf_file: The package configuration file.

    Returns:
      File, the generate package cache file.

    """

    cache_file = hs.actions.declare_file("package.cache", sibling = conf_file)

    # Make the call to ghc-pkg and use the package configuration file
    hs.actions.run(
        inputs = depset(direct = [conf_file]),
        outputs = [cache_file],
        mnemonic = "HaskellRegisterPackage",
        progress_message = "HaskellRegisterPackage {}".format(conf_file.short_path),
        executable = hs.tools.ghc_pkg,
        tools = hs.tools_config.tools_for_ghc_pkg,
        # Registration of a new package consists in,
        #
        # 1. copying the registration file into the package db,
        # 2. performing some validation on the registration file content,
        # 3. recaching, i.e. regenerating the package db cache file.
        #
        # Normally, this is all done by `ghc-pkg register`. But in our
        # case, `ghc-pkg register` is painful, because the validation
        # it performs is slow, somewhat redundant but especially, too
        # strict (see e.g.
        # https://ghc.haskell.org/trac/ghc/ticket/15478). So we do (1)
        # and (3) manually, by copying then calling `ghc-pkg recache`
        # directly.
        #
        # The downside is that we do lose the few validations that
        # `ghc-pkg register` was doing that was useful. e.g. when
        # reexporting modules, validation checks that the source
        # module does exist.
        #
        # TODO Go back to using `ghc-pkg register`. Blocked by
        # https://ghc.haskell.org/trac/ghc/ticket/15478
        arguments = [
            "recache",
            "--package-db={0}".format(conf_file.dirname),
            "-v0",
            "--no-expand-pkgroot",
        ],
        env = {
            "PATH": (";" if hs.toolchain.is_windows else ":").join(posix.paths),
        },
    )

    return cache_file
