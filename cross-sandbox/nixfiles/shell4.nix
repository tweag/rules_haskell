{ haskellNix ? import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/b406bb1d5bed581f651ae18a5d3ed07f47ace2b9.tar.gz) {}
, pkgs ? import haskellNix.sources.nixpkgs haskellNix.nixpkgsArgs
}:
(pkgs.pkgsCross.aarch64-multiplatform.haskell-nix.hackage-project
  { name = "hello";
    version = "1.0.0.2";
	compiler-nix-name = "ghc8102";
  }
).hsPkgs.shellFor {
  buildInputs =
    [ pkgs.buildPackages.ghc-extra-packages.ghc8102.iserv-proxy.components.exes.iserv-proxy
      pkgs.ghc-extra-packages.ghc8102.iserv-proxy.components.exes.iserv-proxy
	];
}
