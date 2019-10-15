load("@bazel_tools//tools/cpp:lib_cc_configure.bzl", "get_cpu_value")
load(
    "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_package",
)

def _unix_nixpkgs(name, packages, **kwargs):
    nixpkgs_package(
        name = name,
        nix_file_content = """
with import <nixpkgs> {{ config = {{}}; overlays = []; }};

let
  # `packages` might include lists, e.g. `stdenv.initialPath` is a list itself,
  # so we need to flatten `packages`.
  flatten = builtins.concatMap (x: if builtins.isList x then x else [x]);
  env = buildEnv {{
    name = "unix-toolchain";
    paths = flatten [ {} ];
  }};
  cmd_glob = "${{env}}/bin/*";
  os = if stdenv.isDarwin then "osx" else "linux";
in

runCommand "bazel-unix-toolchain"
  {{ executable = false;
    # Pointless to do this on a remote machine.
    preferLocalBuild = true;
    allowSubstitutes = false;
  }}
  ''
    n=$out/unix_nixpkgs.bzl
    mkdir -p "$(dirname "$n")"

    cat >>$n <<EOF
    load(
        "@rules_haskell//haskell/private/unix:unix_commands.bzl",
        "unix_commands",
    )
    load(
        "@rules_haskell//haskell/private/unix:unix_toolchain.bzl",
        "unix_toolchain",
    )
    discovered = {{
    EOF
    for cmd in ${{cmd_glob}}; do
        if [[ -x $cmd ]]; then
            echo "    '$(basename $cmd)': '$cmd'," >>$n
        fi
    done
    cat >>$n <<EOF
    }}
    def create_unix_toolchain():
        unix_toolchain(
            name = "nixpkgs_unix",
            **{{
                cmd: discovered[cmd]
                for cmd in unix_commands
                if cmd in discovered
            }}
        )
    EOF
  ''
""".format(" ".join(packages)),
        build_file_content = """
load("//:unix_nixpkgs.bzl", "create_unix_toolchain")
create_unix_toolchain()
""",
        **kwargs
    )

def _unix_nixpkgs_toolchain_impl(repository_ctx):
    cpu = get_cpu_value(repository_ctx)
    repository_ctx.file("BUILD", executable = False, content = """
toolchain(
    name = "nixpkgs_unix_toolchain",
    toolchain = "@{workspace}//:nixpkgs_unix",
    toolchain_type = "@rules_haskell//haskell/private/unix:toolchain_type",
    exec_compatible_with = [
        "@bazel_tools//platforms:x86_64",
        "@bazel_tools//platforms:{os}",
        "@rules_haskell//haskell/platforms:nixpkgs",
    ],
    target_compatible_with = [
        "@bazel_tools//platforms:x86_64",
        "@bazel_tools//platforms:{os}",
    ],
)
    """.format(
        workspace = repository_ctx.attr.workspace,
        os = "osx" if cpu == "darwin" else "linux",
    ))

_unix_nixpkgs_toolchain = repository_rule(
    _unix_nixpkgs_toolchain_impl,
    attrs = {
        "workspace": attr.string(),
    },
)

def unix_nixpkgs(name, packages = ["stdenv.initialPath"], **kwargs):
    """Create a unix toolchain from nixpkgs.

    Loads the given Nix packages, scans them for standard Unix tools, and
    generates a corresponding `unix_toolchain`.

    """
    _unix_nixpkgs(
        name = name,
        packages = packages,
        **kwargs
    )

    # The indirection is required to avoid errors when `nix-build` is not in `PATH`.
    _unix_nixpkgs_toolchain(
        name = name + "_toolchain",
        workspace = name,
    )
    native.register_toolchains(
        "@{}//:nixpkgs_unix_toolchain".format(name + "_toolchain"),
    )
