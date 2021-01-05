
let
#  nativePkgs = import ./nixpkgs.nix native.nixpkgsArgs;
  aarch64Pkgs = import ./nixpkgs.nix { overlays = aarch64.nixpkgsArgs;
  haskellNix = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/b406bb1d5bed581f651ae18a5d3ed07f47ace2b9.tar.gz);
#  native = haskellNix { inherit nativePkgs; };
  aarch64 = haskellNix { pkgs = aarch64Pkgs.pkgsCross.aarch64; };
in
   aarch64.pkgs.pkgCross.aarch64.haskellPackages.ghcWithPackages (ps: with ps; [])
#  aarch64Pkgs.haskellPackages.shellFor {
    # Include only the *local* packages of your project.
    # packages = ps: with ps; [ bazel-cross-sandbox ];
#    packages = ps: with ps; [];

    # You might want some extra tools in the shell (optional).

    # Some common tools can be added with the `tools` argument
    #tools = { cabal = "3.2.0.0"; hlint = "2.2.11"; };
    # See overlays/tools.nix for more details

    # Some you may need to get some other way.
 #   buildInputs = [];

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
  #  exactDeps = true;
  #}
