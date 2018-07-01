{ pkgs ? import ../nixpkgs.nix {} }:

with pkgs;

let haskellPackages = pkgs.haskell.packages.ghc822.override {
      overrides = with pkgs.haskell.lib; self: super: rec {
        lens-labels = super.lens-labels_0_2_0_1;
        proto-lens = super.proto-lens_0_3_1_0;
      };
    };

in haskellPackages.proto-lens-protoc
