# shell.nix
let
  hsPkgs = import ./default.nix;
  pkgsNative = import <nixpkgs> {};
  haskellNix = import (builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/b406bb1d5bed581f651ae18a5d3ed07f47ace2b9.tar.gz") {};

  aarch64Pkgs = import <nixpkgs>  (haskellNix.nixpkgsArgs // {
	    crossSystem = pkgsNative.lib.systems.examples.aarch64-multiplatform;
	});
  aarch64 = hsPkgs { pkgs = aarch64Pkgs; };
in
  aarch64.shellFor {
    # Include only the *local* packages of your project.
    packages = ps: with ps; [ bazel-cross-sandbox ];

    # You might want some extra tools in the shell (optional).

    # Some common tools can be added with the `tools` argument
    tools = { cabal = "3.2.0.0"; hlint = "2.2.11"; };
    # See overlays/tools.nix for more details

    # Some you may need to get some other way.
    buildInputs = [];

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = true;
  }
