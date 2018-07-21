{ pkgs ? import ../nixpkgs.nix {} }:

with pkgs;

haskell.packages.ghc822.ghcWithPackages (p: with p; [
  bytestring
  containers
  data-default-class
  lens-family
  lens-labels
  proto-lens
  text
  ])
