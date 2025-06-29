"""Tests for the `stack_snapshot` module extension."""

load("@bazel_skylib//lib:sets.bzl", "sets")
load("@bazel_skylib//lib:unittest.bzl", "asserts", "unittest")
load("//extensions:stack_snapshot.bzl", "stack_snapshot_testing")
load("//haskell:cabal.bzl", "chop_version")

def _chop_version_test(ctx):
    env = unittest.begin(ctx)

    tests = [
        struct(name = "happy-2.1.5", exp = "happy"),
        struct(name = "happy", exp = "happy"),
        struct(name = "happy-lib-2.1.5", exp = "happy-lib"),
        struct(name = "happy-lib", exp = "happy-lib"),
        struct(name = "ghc-lib-parser-9.12.2.20250421", exp = "ghc-lib-parser"),
        struct(
            msg = "the last component is not a version",
            name = "base64-bytestring",
            exp = "base64-bytestring",
        ),
        struct(
            msg = "only one component, so keep it as is",
            name = "1.0",
            exp = "1.0",
        ),
    ]
    for t in tests:
        msg = getattr(t, "msg", t.name)
        asserts.equals(env, t.exp, chop_version(t.name), msg)

    return unittest.end(env)

chop_version_test = unittest.make(_chop_version_test)

def _package_tag(name, **kwargs):
    """A `stack_snapshot.package` tag, with the same defaults as the tag class."""
    attrs = dict(
        name = name,
        setup_deps = [],
        flags = [],
        extra_deps = [],
        components = ["DETECT_DEFAULT"],
        components_dependencies = {},
        vendored = None,
        hidden = False,
        components_args = {},
    )
    attrs.update(kwargs)
    return struct(**attrs)

def _module(*package_tags):
    return struct(
        name = "test_module",
        version = "1.0",
        tags = struct(package = list(package_tags)),
    )

def _versioned_package_name_test(ctx):
    env = unittest.begin(ctx)

    conf = stack_snapshot_testing.new_packages_conf()
    stack_snapshot_testing.add_packages(
        conf,
        _module(_package_tag(
            "happy-lib-2.1.5",
            components = ["lib", "lib:tabular"],
            components_args = {"lib": "//cabal:empty_library"},
            components_dependencies = {"lib": ["lib:tabular"]},
            extra_deps = ["//deps:extra"],
            setup_deps = ["//deps:setup"],
            flags = ["-flag"],
        )),
        root_or_rules_haskell = True,
    )

    # Only the `packages` argument of `stack_snapshot` takes versions.  All the other arguments use
    # unversioned package names.
    asserts.equals(env, ["happy-lib-2.1.5"], sets.to_list(conf.packages), "packages")
    asserts.equals(env, {"happy-lib": ["lib", "lib:tabular"]}, conf.components, "components")
    asserts.equals(
        env,
        {"happy-lib:lib": "//cabal:empty_library"},
        conf.components_args,
        "components_args",
    )
    asserts.equals(
        env,
        {"happy-lib": json.encode({"lib": ["lib:tabular"]})},
        conf.components_dependencies,
        "components_dependencies",
    )
    asserts.equals(env, {"happy-lib": ["//deps:extra"]}, conf.extra_deps, "extra_deps")
    asserts.equals(env, {"happy-lib": ["//deps:setup"]}, conf.setup_deps, "setup_deps")
    asserts.equals(env, {"happy-lib": ["-flag"]}, conf.flags, "flags")

    return unittest.end(env)

versioned_package_name_test = unittest.make(_versioned_package_name_test)

def _versioned_package_configured_once_test(ctx):
    env = unittest.begin(ctx)

    conf = stack_snapshot_testing.new_packages_conf()
    stack_snapshot_testing.add_packages(
        conf,
        _module(_package_tag("happy-lib-2.1.5", components = ["lib"])),
        root_or_rules_haskell = True,
    )

    # A package is configured by the first module that mentions it, with or without a version.
    stack_snapshot_testing.add_packages(
        conf,
        _module(_package_tag("happy-lib", components = ["lib", "lib:tabular"], hidden = True)),
        root_or_rules_haskell = False,
    )
    asserts.equals(env, {"happy-lib": ["lib"]}, conf.components, "components")

    return unittest.end(env)

versioned_package_configured_once_test = unittest.make(_versioned_package_configured_once_test)

def stack_snapshot_test_suite(name = "stack_snapshot_tests"):
    return unittest.suite(
        name,
        chop_version_test,
        versioned_package_name_test,
        versioned_package_configured_once_test,
    )
