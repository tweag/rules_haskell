{ # Fetch the latest haskell.nix and import its default.nix
  haskellNix ? import (builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/b406bb1d5bed581f651ae18a5d3ed07f47ace2b9.tar.gz") {}

# haskell.nix provides access to the nixpkgs pins which are used by our CI,
# hence you will be more likely to get cache hits when using these.
# But you can also just use your own, e.g. '<nixpkgs>'.
#, nixpkgsSrc ? haskellNix.sources.nixpkgs-2003
,  nixpkgsSrc ? ./nixpkgs.nix

# haskell.nix provides some arguments to be passed to nixpkgs, including some
# patches and also the haskell.nix functionality itself as an overlay.
, nixpkgsArgs ? haskellNix.nixpkgsArgs

# import nixpkgs with overlays
, pkgs ? import nixpkgsSrc nixpkgsArgs
}: pkgs.haskell-nix.project {
  src = ./.;
  # For `cabal.project` based projects specify the GHC version to use.
  compiler-nix-name = "ghc883"; # Not used for `stack.yaml` based projects.
}
