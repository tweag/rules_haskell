let
  # Update with the following command in the repository root:
  # $ nix-shell -p nix-prefetch-git --pure --run '
  #     nix-prefetch-git https://github.com/nixos/nixpkgs-channels \
  #     --rev refs/heads/nixpkgs-unstable' \
  #     > ./nix/nixpkgs/nixpkgs-version.json
  version = builtins.fromJSON (builtins.readFile ./nixpkgs-version.json);
in
import (builtins.fetchTarball {
  name = "nixpkgs-unstable";
  url = "${version.url}/archive/${version.rev}.tar.gz";
  inherit (version) sha256;
})
