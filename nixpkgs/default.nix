{ ... }@args:
let
  # 2023-01-03
  sha256 = "1213j2gvsmzmhpyi2jyn729s9d0gbn56k3d873iknd5ilb2rvpcr";
  rev = "73247c5ab2bc2037d7dd0d310f1fb43ed191edad";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
}) args
