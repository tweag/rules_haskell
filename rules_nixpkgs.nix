{ nixpkgs }:

/*:
  -- @buildFile@ is the bazel @BUILD@ file for the package
  -- @nixPackage@ is the nix derivation we want to add as dependency
  type Package = { buildFile = Derivation; nixPackage = Derivation }
  type PackageSet = { _ =? Package }
*/
let p = nixpkgs; in

let
  prefixLines = pfx: str:
    let
      lines = p.lib.splitString "\n" str;
      prefixedLines = map (l: pfx + l) lines;
    in
    builtins.concatStringsSep "\n" prefixedLines;
  mkNixDep = name: { buildFile, nixPackage } /*: Package */: ''
    native.new_local_repository(
        name = "${name}",
        path = "${nixPackage}",
        build_file = "${buildFile}",
    )'';
  mkWorkspaceFile = packages /*: PackageSet */: p.writeText "WORKSPACE" ''
    def nix_packages():
    ${prefixLines "    " (builtins.concatStringsSep "\n" (p.lib.mapAttrsToList mkNixDep packages))}
  '';
  defaultBUILD = p.writeText "BUILD.default" ''
        package(default_visibility = [ "//visibility:public" ])

        filegroup(
        name = "bin",
        srcs = glob(["bin/*"]),
        )

        filegroup(
        name = "lib",
        srcs = glob(["lib/**/*.so*", "lib/**/*.a"]),
        )

        filegroup(
        name = "include",
        srcs = glob(["include/*.h"]),
        )

  '';
in
{
  buildEnv = args@{
    name /*: String */
  , packages /*: PackageSet */
  , ...
  }:
  p.stdenv.mkDerivation {
    inherit name;

    BAZEL_WORKSPACE = mkWorkspaceFile packages;

    shellHook = ''
      ln -fs $BAZEL_WORKSPACE ./nix-workspace.bzl
    '';
  };
  withDefaultBuild = nixPackage: { buildFile = defaultBUILD; inherit nixPackage; };
}
