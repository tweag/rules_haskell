"""
This module extension gathers packages and configuration from all the modules that use it,
in order to make a unique call to `stack_snapshot` to generate a "stackage" shared repository.

Most of the configuration can only be made by the root module, and
other modules can declare the packages they want installed.

The rules_haskell module itself has more permissions, so that it can
set default configurations for packages as well as set the default
snapshot to use.
"""

load("@bazel_skylib//lib:new_sets.bzl", "sets")
load("@os_info//:os_info.bzl", "cpu_value", "is_darwin", "is_linux", "is_windows")
load("@rules_haskell//haskell:cabal.bzl", _stack_snapshot = "stack_snapshot")

_snapshot_tag = tag_class(
    doc = "The stack snapshot to use.",
    attrs = {
        "name": attr.string(
            doc = """The name of a Stackage snapshot. Incompatible with local_snapshot.""",
        ),
        "local_snapshot": attr.label(
            doc = """ A custom Stack snapshot file, as per the Stack documentation.
            Incompatible with snapshot.
            """,
        ),
    },
)

_stack_snapshot_json_tag = tag_class(
    doc = """ Specify this to use pinned artifacts for generating build targets.""",
    attrs = {
        "label": attr.label(
            doc = " A label to a `stack_snapshot.json` file, e.g. `//:stack_snapshot.json`.",
        ),
        "os": attr.string_list(
            doc = "A list of os compatible with this lockfile. Leave empty if compatible with everything.",
        ),
    },
)

_verbose_tag = tag_class(
    doc = "Whether to show the output of the build.",
    attrs = {
        "value": attr.bool(default = True),
    },
)

_netrc_tag = tag_class(
    doc = """Location of the .netrc file to use for authentication.
        Defaults to `~/.netrc` if present.
    """,
    attrs = {
        "location": attr.string(),
    },
)

_tools_tag = tag_class(
    doc = """Tool dependencies.""",
    attrs = {
        "labels": attr.label_list(),
    },
)

_stack_tag = tag_class(
    doc = """The stack binary to use to enumerate package dependencies.""",
    attrs = {
        "label": attr.label(),
    },
)

_haddock_tag = tag_class(
    doc = "Whether to generate haddock documentation.",
    attrs = {
        "value": attr.bool(default = True),
    },
)

_package_tag = tag_class(
    doc = "Add and configures a package to the snapshot",
    attrs = {
        "name": attr.string(
            mandatory = True,
            doc = "The name of the package to add/configure",
        ),
        "setup_deps": attr.label_list(
            doc = """ Setup dependencies the package, e.g. `cabal-doctest`.
            Only usable by the root module (or rules_haskell itself).
            """,
        ),
        "flags": attr.string_list(
            doc = """List of cabal flags to configure this package.
            Only usable by the root module (or rules_haskell itself).
            For a flag called `flag1`, to enable say `flag1` or `+flag1`, to disable say `-flag1`.
            """,
        ),
        "extra_deps": attr.label_list(
            doc = "Extra dependencies of packages, e.g. system libraries or C/C++ libraries.",
        ),
        "components": attr.string_list(
            doc = """ components: Defines which Cabal components to build for this package.
            Use `lib` for the main library component, `exe:<exe-name>` for an executable component,
            and `lib:<sublib-name>` for a sublibrary.
            `exe` is a short-cut for `exe:<package-name>`. The main library component
            will have the label `@<workspace>//:<package>` as well as the alias `@<workspace>//<package>`, an executable
            component will have the label `@<workspace>-exe//<package>:<exe-name>`,
            and a sublibrary component will have the label `@<workspace>//<package>:<sublib-name>`
            where `<workspace>` is the name of the stackage repository.
            """,
            default = ["DETECT_DEFAULT"],
        ),
        "components_dependencies": attr.string_list_dict(
            doc = """ Internal dependencies between package components.
            The most common case is the following, where the main library of a package depends on sublibraries:

            ```
            {"lib:package-name": ["lib:sublib1", "lib:sublib2"]},
            ```
            """,
        ),
        "vendored": attr.label(
            doc = """Add or override the package with a custom unpacked source distribution.
            The package must contain a Cabal file named <package-name>.cabal in the package root.
            Only usable by the root module (or rules_haskell itself).
            """,
        ),
        "hidden": attr.bool(
            default = False,
            doc = "Configuring a package makes it visible by default, unless `hidden` is set to True",
        ),
        "components_args": attr.string_dict(
            doc = """ components_args: Defines map from component to cabal settings for that package.
            The cabal settings are a target of type `haskell_cabal_args`. Currently handles empty
            libraries and telling the build system to ignore `Setup.hs`.
            Use `lib` for the main library component, `exe:<exe-name>` for an executable component,
            and `lib:<sublib-name>` for a sublibrary.
            `exe` is a short-cut for `exe:<package-name>`.
            Either use the builtin "@rules_haskell//tools/cabal_args:empty_library", or create your own
            if necessary.
            """,
        ),
    },
)

def _assert_unique_tag(tags, tag_name, module):
    """ Errors if the module used the `tag_name` tag more than once"""
    if len(tags) > 1:
        fail(
            """Module "{module_name}~{module_version}" used configuration tag "{tag_name}" more than once""".format(
                module_name = module.name,
                module_version = module.version,
                tag_name = tag_name,
            ),
        )

def _assert_no_root_package_attrs(module, package_tag):
    msg = """Non-root module "{module_name}~{module_version}" tried to use attr "{attr}" which is only valid for root modules."""
    if package_tag.setup_deps:
        fail(msg.format(module_name = module.name, module_version = module.version, attr = "setup_deps"))
    if package_tag.flags:
        fail(msg.format(module_name = module.name, module_version = module.version, attr = "flags"))
    if package_tag.vendored:
        fail(msg.format(module_name = module.name, module_version = module.version, attr = "vendored"))

def _add_packages(conf, module, root_or_rules_haskell):
    """Read the `package` tags from `module` and add the configuration to `conf`"""
    packages_in_module = sets.make()
    for package_tag in module.tags.package:
        package_name = package_tag.name

        # Check that a package is configure at most one time per module
        if sets.contains(packages_in_module, package_name):
            fail("""Module "{module_name}~{module_version}" configured package "{package_name}" multiple times.""".format(
                module_version = module.version,
                module_name = module.name,
                package_name = package_name,
            ))
        sets.insert(packages_in_module, package_name)

        # If the package is already configured, we can only make it visible.
        if sets.contains(conf.configured_packages, package_name):
            if not package_tag.hidden and package_name not in conf.vendored_packages:
                sets.insert(conf.packages, package_name)
            continue
        sets.insert(conf.configured_packages, package_name)

        # We are the first one to configure the package:
        if not package_tag.hidden and not package_tag.vendored:
            sets.insert(conf.packages, package_name)
        if package_tag.components != ["DETECT_DEFAULT"]:
            # Some packages have default components set
            conf.components[package_name] = package_tag.components
        if package_tag.components_args:
            conf.components[package_name] = package_tag.components_args
        if package_tag.components_dependencies:
            conf.components_dependencies[package_name] = json.encode(package_tag.components_dependencies)
        if package_tag.extra_deps:
            conf.extra_deps[package_name] = package_tag.extra_deps
        if root_or_rules_haskell:
            # Only the root module or rules_haskell can modify the
            # following configuration settings. To make the configuration
            # easier to understand.
            if package_tag.setup_deps:
                conf.setup_deps[package_name] = package_tag.setup_deps
            if package_tag.flags:
                conf.flags[package_name] = package_tag.flags
            if package_tag.vendored:
                conf.vendored_packages[package_name] = package_tag.vendored
        else:
            _assert_no_root_package_attrs(module, package_tag)

def _assert_non_rules_haskell_tags(module):
    msg = """Non-root module "{module_name}~{module_version}" tried to use the "{tag}" tag which is only valid for root modules (and rules_haskell)."""

    if module.tags.snapshot:
        fail(msg.format(module_name = module.name, module_version = module.version, tag = "snapshot"))

def _assert_no_root_tags(module):
    msg = """Non-root module "{module_name}~{module_version}" tried to use the "{tag}" tag which is only valid for root modules."""

    if module.tags.stack_snapshot_json:
        fail(msg.format(module_name = module.name, module_version = module.version, tag = "stack_snapshot_json"))

    if module.tags.verbose:
        fail(msg.format(module_name = module.name, module_version = module.version, tag = "verbose"))

    if module.tags.netrc:
        fail(msg.format(module_name = module.name, module_version = module.version, tag = "netrc"))

    if module.tags.tools:
        fail(msg.format(module_name = module.name, module_version = module.version, tag = "tools"))

    if module.tags.stack:
        fail(msg.format(module_name = module.name, module_version = module.version, tag = "stack"))

    if module.tags.haddock:
        fail(msg.format(module_name = module.name, module_version = module.version, tag = "haddock"))

def _stack_snapshot_impl(mctx):
    root_module = None
    rules_haskell_modules = []
    kwargs = {}
    for module in mctx.modules:
        if module.is_root:
            root_module = module
        if module.name == "rules_haskell":
            rules_haskell_modules.append(module)
        if module in [root_module] + rules_haskell_modules:
            # Most modules can only add the packages they want to use to the snapshot,
            # but only the root module can configure all the settings.
            # rules_haskell can also set the snapshot to be used as a default.
            if module.tags.snapshot:
                _assert_unique_tag(module.tags.snapshot, "snapshot", module)
                snapshot_tag = module.tags.snapshot[0]
                if "snapshot" not in kwargs and "local_snapshot" not in kwargs:
                    if snapshot_tag.local_snapshot:
                        kwargs["local_snapshot"] = snapshot_tag.local_snapshot
                    if snapshot_tag.name:
                        kwargs["snapshot"] = snapshot_tag.name
        else:
            _assert_non_rules_haskell_tags(module)
        if module == root_module:
            for stack_snapshot_json_tag in module.tags.stack_snapshot_json:
                # If the os list is empty (the default value), the file is compatible with all OSs.
                # Otherwise it is compatible with the OSs/cpu_values listed
                os_list = stack_snapshot_json_tag.os
                if (
                    not os_list or
                    any([
                        is_linux and "linux" in os_list,
                        is_windows and "windows" in os_list,
                        is_darwin and [x for x in os_list if x in ["macos", "osx", "mac"]],
                        cpu_value in os_list,
                    ])
                ):
                    kwargs["stack_snapshot_json"] = stack_snapshot_json_tag.label
                    break

            if module.tags.verbose:
                _assert_unique_tag(module.tags.verbose, "verbose", module)
                verbose_tag = module.tags.verbose[0]
                kwargs["verbose"] = verbose_tag.value
            if module.tags.netrc:
                _assert_unique_tag(module.tags.netrc, "netrc", module)
                netrc_tag = module.tags.netrc[0]
                kwargs["netrc"] = netrc_tag.location
            if module.tags.tools:
                _assert_unique_tag(module.tags.tools, "tools", module)
                tools_tag = module.tags.tools[0]
                kwargs["tools"] = tools_tag.labels
            if module.tags.stack:
                _assert_unique_tag(module.tags.stack, "stack", module)
                stack_tag = module.tags.stack[0]
                kwargs["stack"] = stack_tag.label
            if module.tags.haddock:
                _assert_unique_tag(module.tags.haddock, "haddock", module)
                haddock_tag = module.tags.haddock[0]
                kwargs["haddock"] = haddock_tag.label
        else:
            _assert_no_root_tags(module)

    packages_conf = struct(
        configured_packages = sets.make(),
        packages = sets.make(),  # "packages" argument of stack_snapshot, must not contain hidden and vendored packages.
        setup_deps = {},
        flags = {},
        extra_deps = {},
        components = {},
        components_dependencies = {},
        components_args = {},
        vendored_packages = {},
    )

    # For the the configuration of packages, the root module takes
    # precedence, then rules_haskell modules, then the other modules (with
    # lower privilege).
    if root_module:
        _add_packages(packages_conf, root_module, root_or_rules_haskell = True)
    for module in rules_haskell_modules:
        if module != root_module:
            _add_packages(packages_conf, module, root_or_rules_haskell = True)
    for module in mctx.modules:
        if module != root_module and module not in rules_haskell_modules:
            _add_packages(packages_conf, module, root_or_rules_haskell = False)

    kwargs["packages"] = sets.to_list(packages_conf.packages)
    kwargs["setup_deps"] = {
        package: [str(label) for label in labels]
        for package, labels in packages_conf.setup_deps.items()
    }
    kwargs["flags"] = packages_conf.flags
    kwargs["components"] = packages_conf.components
    kwargs["components_dependencies"] = packages_conf.components_dependencies
    kwargs["components_args"] = packages_conf.components_args
    kwargs["extra_deps"] = packages_conf.extra_deps
    kwargs["vendored_packages"] = packages_conf.vendored_packages
    kwargs["name"] = "stackage"
    _stack_snapshot(**kwargs)

stack_snapshot = module_extension(
    implementation = _stack_snapshot_impl,
    tag_classes = {
        "package": _package_tag,
        "snapshot": _snapshot_tag,
        "stack_snapshot_json": _stack_snapshot_json_tag,
        "verbose": _verbose_tag,
        "netrc": _netrc_tag,
        "tools": _tools_tag,
        "stack": _stack_tag,
        "haddock": _haddock_tag,
    },
)
