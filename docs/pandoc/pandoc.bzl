load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl", "nixpkgs_package")

def _pandoc_toolchain_impl(ctx):
    return [platform_common.ToolchainInfo(
        pandoc = ctx.attr.pandoc,
    )]

pandoc_toolchain = rule(
    attrs = {
        "pandoc": attr.label(
            doc = "The pandoc executable.",
            allow_single_file = True,
            mandatory = True,
        ),
    },
    doc = "A toolchain holding the pandoc binary.",
    implementation = _pandoc_toolchain_impl,
)

def _pandoc_impl(ctx):
    toolchain = ctx.toolchains["@rules_haskell//docs/pandoc:toolchain_type"]
    return [DefaultInfo(
        files = toolchain.pandoc.files,
    )]

pandoc = rule(
    doc = "The pandoc binary exposed by the pandoc toolchain",
    implementation = _pandoc_impl,
    toolchains = ["@rules_haskell//docs/pandoc:toolchain_type"],
)

def nixpkgs_pandoc_configure(
        name = "nixpkgs_pandoc",
        attribute_path = "pandoc",
        **kwargs):
    nixpkgs_package(
        name = name,
        attribute_path = attribute_path,
        build_file = None,
        build_file_content = """\
load("@rules_haskell//docs/pandoc:pandoc.bzl", "pandoc_toolchain")
pandoc_toolchain(
    name = "pandoc_toolchain",
    pandoc = ":bin/pandoc",
    visibility = ["//visibility:public"],
)
""",
        **kwargs
    )

_pandoc_bindists = {
    "linux": {
        "urls": ["https://github.com/jgm/pandoc/releases/download/2.7.3/pandoc-2.7.3-linux.tar.gz"],
        "strip_prefix": "pandoc-2.7.3/",
        "sha256": "eb775fd42ec50329004d00f0c9b13076e707cdd44745517c8ce2581fb8abdb75",
    },
    "macos": {
        "urls": ["https://github.com/jgm/pandoc/releases/download/2.7.3/pandoc-2.7.3-macOS.zip"],
        "strip_prefix": "pandoc-2.7.3/",
        "sha256": "fb93800c90f3fab05dbd418ee6180d086b619c9179b822ddfecb608874554ff0",
    },
}

def import_pandoc_bindists():
    for (os, kwargs) in _pandoc_bindists.items():
        http_archive(
            name = "%s_pandoc" % os,
            build_file_content = """\
load("@rules_haskell//docs/pandoc:pandoc.bzl", "pandoc_toolchain")
pandoc_toolchain(
    name = "pandoc_toolchain",
    pandoc = ":bin/pandoc",
    visibility = ["//visibility:public"],
)
""",
            **kwargs
        )
