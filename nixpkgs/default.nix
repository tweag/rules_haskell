{ ... }@args:
let
  # 2024-01-09
  sha256 = "sha256:1y4mcj6w08ns5151rdc85gv53kbwzcj6avp603cvjgkf98szhlwc";
  rev = "a6671578ffa6336bfd1238f1b2a00662b5ab1b34";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
}) args
