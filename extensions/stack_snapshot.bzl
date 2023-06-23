"""
The `new` tags are selected in order to create the snapshots
only one `new` tag is considered for each stack_snapshot `name`.
We follow the iteration order of bazel over modules, the root modules being first.
The first module to declare a snapshot with `new` is the owner of the snapshot.

They are two kinds of additional configurations:
  - if the root module is not the owner of a snapshot, it can use the `update_` tag to only modify some of the setting,
    but keep
    (for instance to set `envrc` or the `verbose` mode)


  - package configurations:
    - everyone can add packages and configure `components` and `component dependencies`.
      (if someone with higher priority already added the package, the configuration is ignored)
    - only the root module or the owner of the stack_snapshot can configure: (setup_deps, flags, ...)
"""

load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")
load("@bazel_skylib//lib:new_sets.bzl", "sets")

# load("@os_info//:os_info.bzl", "is_windows")
load("@bazel_tools//tools/cpp:lib_cc_configure.bzl", "get_cpu_value")

_config_from_file_tag = tag_class(
    attrs = {
        "file": attr.label(),
    },
)

_config_tag = tag_class(
    attrs = {
        "name": attr.string(),
        "snapshot": attr.string(),
        "local_snapshot": attr.label(),
        "stack_snapshot_json": attr.label(),
        "packages": attr.string_list(),
        # "setup_deps": attr.label_keyed_string_dict(),
        # "extra_deps": attr.label_keyed_string_dict(),
        # "vendored_packages": attr.label_keyed_string_dict(),  # Only first one or ignored
        "verbose": attr.bool(default = True),
        "netrc": attr.string(),
        "tools": attr.label_list(),
        "stack": attr.label(),
        "haddock": attr.bool(default = True),
        # "toolchain_libraries": attr.string_list(), read them from a file since they can
    },
    doc = "create/configure a new stack_snapshot",
)

# ignored if someone already registered the package.
_package_tag = tag_class(
    attrs = {
        "snapshot_name": attr.string(
            mandatory = True,
            doc = "The name of the stack snapshot to configure",
        ),
        "name": attr.string(
            mandatory = True,
            doc = "The name of the package to add/configure",
        ),
        "setup_deps": attr.label_list(
            doc = """See setup_deps for the package. Ignored it you are not the owner or root module of the stack snapshot. Because you could modify a dependency """,
        ),
        "flags": attr.string_list(
            doc = "ignore if you are not the owner or root module",
        ),
        "extra_deps": attr.label_list(
            doc = """See extra_deps for the package.""",
        ),
        "components": attr.string_list(
            doc = """See components""",
        ),
        "components_dependencies": attr.string_list_dict(
            doc = """See components_dependencies""",
        ),
        "vendored": attr.label(
            doc = "",
        ),
    },
)

_update_tools_tag = tag_class(
    attrs = {
        "tools": attr.label_list(),
        "snapshot_name": attr.string(
            mandatory = True,
        ),
    },
)

# # Setup deps needs to be labels to be resolved in the context of the calling module.
# # (or strings with the canonical label which requires the user to build it in a module extension from the calling module)
# # S

# # [TODO] change name because used for extra deps as well
# def _convert_setup_deps(setup_deps):
#     res = {}
#     for (setup_dep, packages_str) in setup_deps.items():
#         packages = json.decode(packages_str)
#         print("setup_dep=", setup_dep)
#         print("packages=", packages)
#         for package in packages:
#             res.setdefault(package, []).append(setup_dep)
#     return res

# _convert_extra_deps = _convert_setup_deps

def _stack_snapshots_impl(mctx):
    first_snapshot = True
    snapshots = {}
    owners = {}
    setup_dep = {}
    flags = {}
    extra_deps = {}
    components = {}
    components_dependencies = {}
    is_windows = get_cpu_value(mctx) == "x64_windows"
    for module in mctx.modules:
        for config_tag in module.tags.config:
            snapshot_name = config_tag.name
            if snapshot_name not in snapshots:
                # todo snapshots[snapshot_name] = conf separatly
                conf = snapshots.setdefault(snapshot_name, {})
                conf["kwargs"] = {
                    "name": snapshot_name,
                    "snapshot": config_tag.snapshot,
                    "local_snapshot": config_tag.local_snapshot,

                    # Does stack_snapshot_json does not work on windows ?
                    # We test if we are on windows here because it cannot be done in the MODULE file.
                    "stack_snapshot_json": config_tag.stack_snapshot_json if not is_windows else None,
                    # "packages": config_tag.packages,
                    # "vendored_packages": config_tag.vendored_packages,
                    "verbose": config_tag.verbose,
                    "netrc": config_tag.netrc,
                    "tools": config_tag.tools,
                    "stack": config_tag.stack,
                    "haddock": config_tag.haddock,
                    "setup_stack": first_snapshot,
                }
                first_snapshot = False
                conf["owner"] = (module.name, module.version)
                conf["packages"] = sets.make(config_tag.packages)

                # conf["configured_packages"] = sets.make(config_tag.packages)
                conf["setup_deps"] = {}
                conf["flags"] = {}
                conf["extra_deps"] = {}
                conf["components"] = {}
                conf["components_dependencies"] = {}
                conf["vendored_packages"] = {}
                packages_tags_by_owner_module = sets.make()

                # Process `package` tags by the owner module now, as they
                # take precedence over (non root) modules that may
                # appear before in the module iteration.
                for package_tag in module.tags.package:
                    if package_tag.snapshot_name == snapshot_name:
                        package_name = package_tag.name
                        if sets.contains(packages_tags_by_owner_module, package_name):
                            fail("""Module "{module_name}~{module_version}" configured package "{package_name}" of stack_snapshot "{snapshot_name}" multiple times.""".format(
                                module_version = module.version,
                                module_name = module.name,
                                package_name = package_name,
                                snapshot_name = snapshot_name,
                            ))
                        sets.insert(packages_tags_by_owner_module, package_name)
                        sets.insert(conf["packages"], package_name)
                        conf["setup_deps"][package_name] = package_tag.setup_deps
                        conf["flags"][package_name] = package_tag.flags
                        conf["extra_deps"][package_name] = package_tag.extra_deps
                        if package_tag.vendored:
                            conf["vendored_packages"][package_name] = package_tag.vendored
                        if package_tag.components:
                            # specifying an empty list of components may prevent using the default ones
                            conf["components"][package_name] = package_tag.components

                        # TODO: should we test for emptiness ?
                        conf["components_dependencies"][package_name] = json.encode(package_tag.components_dependencies)

    # Process package tags of non owner modules
    for module in mctx.modules:
        packages_registered_by_module = sets.make()
        for package_tag in module.tags.package:
            package_name = package_tag.name
            snapshot_name = package_tag.snapshot_name
            if sets.contains(packages_registered_by_module, (snapshot_name, package_name)):
                fail("""Module "{module_name}~{module_version}" configured package "{package_name}" of stack_snapshot "{snapshot_name}" multiple times.""".format(
                    module_version = module.version,
                    module_name = module.name,
                    package_name = package_name,
                    snapshot_name = snapshot_name,
                ))

            # TODO: do this here ?
            sets.insert(packages_registered_by_module, (snapshot_name, package_name))

            # We already processed the package tags for the owner module
            if (module.name, module.version) != snapshots[snapshot_name]["owner"]:
                conf = snapshots[snapshot_name]
                print("module=", module.name)
                print("package_name=", package_name)
                print("packages=", conf["packages"])
                print("is_root=", module.is_root)
                print("package_tag=", package_tag.setup_deps)
                if sets.contains(conf["packages"], package_name) and not module.is_root:
                    # Only the root module can override the configuration of an existing package.
                    continue
                sets.insert(conf["packages"], package_name)
                conf["extra_deps"][package_name] = package_tag.extra_deps
                if package_tag.components:
                    # specifying an empty list of components may prevent using the default ones
                    conf["components"][package_name] = package_tag.components

                # TODO: should we test for emptiness ?
                conf["components_dependencies"][package_name] = json.encode(package_tag.components_dependencies)
                if module.is_root:
                    # We do not want module which is neither owner nor root to modify the behavior of the snapshot to much.
                    # Even if this package is not declared elsewhere if could be a dependency
                    conf["setup_deps"][package_name] = package_tag.setup_deps
                    conf["flags"][package_name] = package_tag.flags
                    if package_tag.vendored:
                        conf["vendored_packages"][package_name] = package_tag.vendored
                    print("after=", conf["setup_deps"][package_name])

    print("config_tag", config_tag.netrc == None)
    print(snapshots)

    # Update tools
    for module in mctx.modules:
        if module.is_root:
            for update_tools_tag in module.tags.update_tools:
                snapshot_name = update_tools_tag.snapshot_name
                if snapshot_name not in snapshots:
                    fail("""module "{module_name}~{module_version}" used update_tools tag with non existent snapshot_name: {snapshot_name}""".format(
                        module_name = module.name,
                        module_version = module.version,
                        snapshot_name = snapshot_name,
                    ))
                snapshots[snapshot_name]["kwargs"]["tools"] = update_tools_tag.tools

    for snapshot, conf in snapshots.items():
        kwargs = conf["kwargs"]
        kwargs["packages"] = sets.to_list(
            sets.difference(
                conf["packages"],
                sets.make(conf["vendored_packages"]),
            ),
        )
        kwargs["setup_deps"] = {
            package: [str(label) for label in labels]
            for package, labels in conf["setup_deps"].items()
        }
        kwargs["flags"] = conf["flags"]
        kwargs["components"] = conf["components"]
        kwargs["components_dependencies"] = conf["components_dependencies"]
        kwargs["extra_deps"] = conf["extra_deps"]
        kwargs["vendored_packages"] = conf["vendored_packages"]
        print("kwars=", kwargs)
        stack_snapshot(**kwargs)

stack_snapshots = module_extension(
    implementation = _stack_snapshots_impl,
    tag_classes = {
        "config": _config_tag,
        "package": _package_tag,
        "update_tools": _update_tools_tag,
    },
)
