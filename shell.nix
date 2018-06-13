{ pkgs ? import ./nixpkgs.nix {} }:

with pkgs;

mkShell {
  buildInputs = [
    binutils
    go
    which
    python
    nix
    bazel
  ];
}
