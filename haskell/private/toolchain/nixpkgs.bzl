"""Definitions for building toolchains using Nix packages"""

load(":private/toolchain/cc.bzl", "cc_configure_custom")
load(":private/toolchain/core.bzl", "haskell_toolchain")

load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl", "nixpkgs_package")

def haskell_nixpkgs_toolchain(
    name,
    ghc_version,
    repository = "@nixpkgs",
    namespace = None,
    compiler_flags = [],
    **kwargs):

    namespace = namespace + "_" if namespace else name + "_"
    config = _haskell_nixpkgs_toolchain_defaults(ghc_version, kwargs)

    nixpkgs_package(
        name = config["patchelf_package_name"],
        repository = repository,
        attribute_path = config["patchelf_attribute_path"],
        build_file_content = """
package(default_visibility = ["//visibility:public"])

sh_binary(
    name = "patchelf",
    srcs = ["bin/patchelf"],
)
""",
    )

    nixpkgs_package(
        name = config["gcc_package_name"],
        repository = repository,
        attribute_path = config["gcc_attribute_path"],
    )

    nixpkgs_package(
        name = config["gcc_lib_package_name"],
        repository = repository,
        attribute_path = config["gcc_lib_attribute_path"],
        build_file_content = """
package(default_visibility = ["//visibility:public"])

load("@io_tweag_rules_haskell://haskell:toolchain/nixpkgs.bzl", "haskell_nixpkgs_patched_solib")
haskell_nixpkgs_patched_solib(
    name = "patched_stdcpp",
    lib_name = "stdc++",
    patchelf = {patchelf},
)
""".format(patchelf = "@" + config["patchelf_package_name"] + "//:patchelf"),
    )

    nixpkgs_package(
        name = config["binutils_package_name"],
        repository = repository,
        attribute_path = config["binutils_attribute_path"],
    )

    cc_configure_custom(
        name = config["custom_cc_name"],
        gcc = "@{gcc}//:bin/cc".format(gcc = config["gcc_package_name"]),
        ld = "@{binutils}//:bin/ld".format(binutils = config["binutils_package_name"]),
    )

    nixpkgs_package(
        name = config["c2hs_package_name"],
        repository = repository,
        attribute_path = config["c2hs_attribute_path"],
    )

    nixpkgs_package(
        name = config["ghc_package_name"],
        repository = repository,
        attribute_path = config["ghc_attribute_path"],
        build_file_content = """
package(default_visibility = ["//visibility:public"])

load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_toolchain")

# Provide the GHC executable separately, so that it can be used in a
# repository rule to compile Hazel.  (Repository rules can't use "generated" files,
# including filegroups, but are allowed to use files provided via
# `exports_files`.
exports_files(["bin/ghc"])

filegroup(
  name = "bin",
  srcs = glob([
    "bin/*",
  ]),
)

filegroup(
  name = "include",
  srcs = glob(["include/*.h"]),
)

filegroup(
  name = "lib",
  srcs = glob(["lib/**/*.so*", "lib/**/*.a"]),
)

cc_library(
  name = "threaded-rts",
  srcs = glob(["lib/ghc-*/rts/libHSrts_thr-ghc*." + ext for ext in ["so", "dylib"]]),
  hdrs = glob(["lib/ghc-*/include/**/*.h"]),
  strip_include_prefix = glob(["lib/ghc-*/include"], exclude_directories=0)[0],
)

# TODO: detect this more automatically.
cc_library(
  name = "unix-includes",
  hdrs = glob(["lib/ghc-*/unix-*/include/*.h"]),
  strip_include_prefix = glob(["lib/ghc-*/unix-*/include"], exclude_directories=0)[0],
)

haskell_toolchain(
    name = "ghc",
    c2hs = "@{c2hs}//:bin",
    version = "{ghc_version}",
    tools = ":bin",
    compiler_flags = {compiler_flags},
)
""".format(
        c2hs = config["c2hs_package_name"],
        ghc_version = ghc_version,
        compiler_flags = "[\"" + "\", \"".join(compiler_flags) + "\"]",
    ))

    native.register_toolchains("@{ghc}//:ghc".format(ghc = config["ghc_package_name"]))

_HASKELL_NIXPKGS_TOOLCHAIN_DEFAULTS = {
    "patchelf_package_name": "patchelf",
    "patchelf_attribute_path": "patchelf",
    "gcc_package_name": "gcc",
    "gcc_attribute_path": "gcc",
    "gcc_lib_package_name": "gcc_lib",
    "gcc_lib_attribute_path": "gcc-unwrapped.lib",
    "binutils_package_name": "binutils",
    "binutils_attribute_path": "binutils",
    "custom_cc_name": "cc",
    "c2hs_package_name": "c2hs",
    "ghc_package_name": "ghc",
}

_HASKELL_NIXPKGS_TOOLCHAIN_DEFAULTS_BY_VERSION = {
    "8.2.2": {
      "c2hs_attribute_path": "haskell.packages.ghc822.c2hs",
      "ghc_attribute_path": "haskell.packages.ghc822.ghc",
    },
    "8.4.3": {
      "c2hs_attribute_path": "haskell.packages.ghc843.c2hs",
      "ghc_attribute_path": "haskell.packages.ghc843.ghc",
    },
}

def _haskell_nixpkgs_toolchain_defaults(ghc_version, overrides):
    config = {}
    config.update(_HASKELL_NIXPKGS_TOOLCHAIN_DEFAULTS)

    if ghc_version in _HASKELL_NIXPKGS_TOOLCHAIN_DEFAULTS_BY_VERSION:
        config.update(_HASKELL_NIXPKGS_TOOLCHAIN_DEFAULTS_BY_VERSION[ghc_version])

    for key, value in config.items():
        config[key] = overrides.pop(key, value)

    if overrides:
        fail("Invalid keyword arguments passed to haskell_nixpkgs_toolchain: {}".format(
            ", ".join(overrides.keys())))

    if "c2hs_attribute_path" not in config or "ghc_version" not in config:
        fail("""

You must either call haskell_nixpkgs_toolchain with a supported ghc_version or
explicitly provide nixpkgs attribute paths for c2hs and GHC.

* You can provide a supported ghc_version as follows:

  haskell_nixpkgs_toolchain(
      ...,
      ghc_version = "8.2.2",
      ...,
  )

  Supported versions are:
      * {supported_ghc_versions}

* You can provide explicit nixpkgs attribute paths for c2hs and GHC as follows:

  haskell_nixpkgs_toolchain(
      ...,
      c2hs_attribute_path = "haskell.packages.ghc822.c2hs",
      ghc_attribute_path = "haskell.packages.ghc822.ghc",
      ...,
  )

""".format(
                supported_ghc_versions = "\n      * ".join(_HASKELL_NIXPKGS_TOOLCHAIN_DEFAULTS_BY_VERSION.keys())
            ))

    return config

def _haskell_nixpkgs_patched_solib_impl(ctx):
    src = ctx.file.src
    if ctx.attr.is_darwin:
        output = ctx.actions.declare_file("lib" + ctx.attr.name + ".dylib")
        ctx.actions.run_shell(
            inputs = [src],
            outputs = [output],
            progress_message = "Patching dylib %s" % ctx.label,
            command = "cp {src} {out}".format(
                src = src.path,
                out = output.path,
                ),
            )
    else:
        output = ctx.actions.declare_file("lib" + ctx.label.name + ".so")
        ctx.actions.run_shell(
            inputs = [src, ctx.executable._patchelf],
            outputs = [output],
            progress_message = "Patching solib %s" % ctx.label,
            command = ("cp {src} {out} && chmod +w {out} && "
                      + "{patchelf} --set-soname {outname} {out}").format(
                        src = src.path,
                        out = output.path,
                        patchelf = ctx.executable._patchelf.path,
                        outname = output.basename,
                        ),
            )

    return [DefaultInfo(files=depset([output]))]

_haskell_nixpkgs_patched_solib = rule(
    _haskell_nixpkgs_patched_solib_impl,
    attrs = {
        "src": attr.label(allow_files = True, single_file = True),
        "_patchelf": attr.label(
            default = Label("@patchelf//:patchelf"),
            executable = True,
            cfg = "host",
            ),
        "is_darwin": attr.bool(),
        },
    )

def haskell_nixpkgs_patched_solib(name, lib_name):
    _haskell_nixpkgs_patched_solib(
        name = name,
        src = native.glob(["lib/lib" + lib_name + ext for ext in [".so", ".dylib"]])[0],
        is_darwin = select({
            "@bazel_tools//src/conditions:darwin": True,
            "//conditions:default": False,
            }),
        )
