let
  # 2020-07-09
  sha256 = "1vi3wbvlvpd4200swd3594vps1fsnd7775mgzm3nnfs1imzkg00i";
  rev = "1d8018068278a717771e9ec4054dff1ebd3252b0";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
