{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let haskellPackages = pkgs.haskell.packages.ghc822.override {
      overrides = with pkgs.haskell.lib; self: super: rec {
        lens-labels = super.lens-labels_0_2_0_0;
        proto-lens = super.proto-lens_0_3_0_0;
      };
    };

in haskellPackages.ghcWithPackages (p: with p; [
  bytestring
  containers
  data-default-class
  lens-family
  lens-labels
  proto-lens
  text
  ])
