{ pkgs ? import ./nixpkgs.nix {} }:

with pkgs;

mkShell {
  buildInputs = [
    binutils
    go
    nix
    which
    python
    nix
    bazel
  ];
}
