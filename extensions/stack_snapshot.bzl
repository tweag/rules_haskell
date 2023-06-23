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

_config_from_file_tag = tag_class(
    attrs = {
        "file": attr.label(),
    },
)

# Only first one ?
_config_tag = tag_class(
    attrs = {
        "name": attr.string(),
        "snapshot": attr.string(),
        "local_snapshot": attr.label(),
        "stack_snapshot_json": attr.label(),
        "packages": attr.string_list(),
        # "setup_deps": attr.label_keyed_string_dict(),
        # "extra_deps": attr.label_keyed_string_dict(),
        "vendored_packages": attr.label_keyed_string_dict(),  # Only first one or ignored
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
        "snapshot": attr.string(
            doc = "The name of the stack snapshot to configure",
        ),
        "name": attr.string(
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
    },
)

# Setup deps needs to be labels to be resolved in the context of the calling module.
# (or strings with the canonical label which requires the user to build it in a module extension from the calling module)
# S

# [TODO] change name because used for extra deps as well
def _convert_setup_deps(setup_deps):
    res = {}
    for (setup_dep, packages_str) in setup_deps.items():
        packages = json.decode(packages_str)
        print("setup_dep=", setup_dep)
        print("packages=", packages)
        for package in packages:
            res.setdefault(package, []).append(setup_dep)
    return res

_convert_extra_deps = _convert_setup_deps

def _stack_snapshots_impl(mctx):
    first_snapshot = True
    snapshots = {}
    for module in mctx.modules:
        for config_tag in module.tags.config:
            if config_tag.name not in snapshots:
                snapshots[config_tag.name] = {
                    "name": config_tag.name,
                    "snapshot": config_tag.snapshot,
                    "local_snapshot": config_tag.local_snapshot,
                    "stack_snapshot_json": config_tag.stack_snapshot_json,
                    "packages": config_tag.packages,
                    "vendored_packages": config_tag.vendored_packages,
                    "verbose": config_tag.verbose,
                    "netrc": config_tag.netrc,
                    "tools": config_tag.tools,
                    "stack": config_tag.stack,
                    "haddock": config_tag.haddock,
                    "setup_stack": first_snapshot,
                }
                first_snapshot = False

            # setup_deps = _convert_setup_deps(config_tag.setup_deps)
            # extra_deps = _convert_setup_deps(config_tag.extra_deps)
            print("config_tag", config_tag.netrc == None)
            print(snapshots)
    for snapshot, kwargs in snapshots.items():
        stack_snapshot(**kwargs)

stack_snapshots = module_extension(
    implementation = _stack_snapshots_impl,
    tag_classes = {
        "config": _config_tag,
        "package": _package_tag,
    },
)
