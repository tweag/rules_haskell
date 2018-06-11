{ pkgs ? import ./nixpkgs.nix {} }:

with pkgs;

mkShell {
  buildInputs = [
    binutils
    ghc
    go
    which
    python
    nix
    bazel
  ];
}
