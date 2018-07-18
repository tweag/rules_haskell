{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let haskellPackages = pkgs.haskell.packages.ghc822.override {
      overrides = with pkgs.haskell.lib; self: super: rec {
        lens-labels = super.lens-labels_0_2_0_1;
        proto-lens = super.proto-lens_0_3_1_0;
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

  # mtl is imported without documentation for testing purpose (see //tests/haddock:doc_broken_deps)
  (haskell.lib.dontCheck (haskell.lib.dontHaddock mtl))
  ])
