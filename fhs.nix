/*

This FHS env allows bazel to run on nixos:

$ nix-shell ./fhs.nix

then, `bazel build //something` will work without any other configuration.

NOTE: for an unknown (yet) reason, part of bazel cache is invalidated if the nix-shell is re-run.

*/

{ pkgs ? import <nixpkgs> {} }:

(pkgs.buildFHSUserEnv {
  name = "rules-haskell-env";
  targetPkgs = pkgs: (with pkgs;
    [ bazel nix binutils go which python27]);
}).env
