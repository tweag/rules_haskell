{ pkgs ? import ./nixpkgs.nix {} }:

with pkgs;

mkShell {
  buildInputs = [
    binutils
    # XXX Prepopulating the shell with ghc to workaround OOM conditions
    # caused by https://github.com/NixOS/nix/issues/1969.
    ghc
    go
    which
    python
    nix
    bazel
  ];
}
