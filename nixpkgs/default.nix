{ ... }@args:
let
  # 2023-11-30
  sha256 = "sha256:064hngn9lwwi30m62ilqhws10h9yh6rnpvxcc205bbh8pzrhp5sc";
  rev = "89b57cd1828e12fadbb4b0025f98dc442283d030";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
}) args
