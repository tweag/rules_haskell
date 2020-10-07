{...}:
let
  # 2020-10-07
  sha256 = "sha256:0fcqpsy6y7dgn0y0wgpa56gsg0b0p8avlpjrd79fp4mp9bl18nda";
  rev = "502845c3e31ef3de0e424f3fcb09217df2ce6df6";
  overlay = self: super: let inherit (self) fetchgit fetchpatch; in {
    haskell = super.haskell // {
      packages = super.haskell.packages // {
        ghcHEAD = super.haskell.packages.ghcHEAD.override {
          overrides = self: super: {
            ghcPatched = (super.ghc.override { version = "8.10.2"; }).overrideAttrs (attrs: {
              src = fetchgit {
                url = "https://gitlab.haskell.org/ghc/ghc.git/";
                rev = "ghc-8.10.2-release";
                sha256 = "sha256:0vhp8a4d4n0c5n85z0jr8s95z8s8frh9mv0n3d03gmi76c39pvid";
              };
              patches = attrs.patches or [] ++ [
                (fetchpatch {
                  url = "https://gitlab.haskell.org/ghc/ghc/-/commit/fac083e7ac8a37b61a4082bbbca2848e52fd1bb2.patch";
                  sha256 = "sha256:0d2l9x9pxpagjnb4l6f9irqvpd7542wk230dyjnabgrv5k1b1fn5";
                })
              ];
            });
          };
        };
      };
    };
  };
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
}) { config = {}; overlays = [overlay]; }
