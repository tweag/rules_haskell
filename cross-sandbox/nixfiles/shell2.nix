
let
  pkgs = import ./nixpkgs.nix {};
  haskellNix = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz);
  native = haskellNix { inherit pkgs; };
in
  native.haskellPackages.bench.components.exes.bench
