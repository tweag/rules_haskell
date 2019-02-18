def _is_nix_platform(repository_ctx):
    if repository_ctx.execute(["nix-build", "--version"]).return_code == 0:
        nix_in_path = True
    else:
        nix_in_path = False
    return nix_in_path

def _gen_imports_impl(repository_ctx):
    repository_ctx.file("BUILD", "")

    nix_file_content = """
load(
    "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_package",
)

def import_gmp():
    nixpkgs_package(
        name = "gmp",
        fail_not_supported = False,
        build_file_content = {multiline}
package(default_visibility = ["//visibility:public"])

filegroup(
    name = "lib",
    srcs = glob(["lib/**/*.so*", "lib/**/*.dylib", "lib/**/*.a"]),
)

cc_library(
    name = "gmp",
    linkstatic = 1,
    srcs = [":lib"],
)
    {multiline},
        repository = "@nixpkgs",
    )


    """.format(
        multiline = '"""',
    )

    no_nix_file_content = """
def _import_gmp_impl(repository_ctx):
    repository_ctx.file("BUILD", 'filegroup(name="gmp", srcs = ["foo.h"], visibility = ["//visibility:public"])')
    repository_ctx.file("foo.h", "")

_import_gmp = repository_rule(
    implementation = _import_gmp_impl,
    attrs = dict(),
)

def import_gmp():
    _import_gmp(name = "gmp")
"""

    if _is_nix_platform(repository_ctx):
        file_content = nix_file_content
    else:
        file_content = no_nix_file_content

    repository_ctx.file("imports.bzl", file_content)

_gen_imports = repository_rule(
    implementation = _gen_imports_impl,
    attrs = dict(),
)

def gen_imports(name):
    _gen_imports(
        name = name,
    )
